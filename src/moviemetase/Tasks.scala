package moviemetase

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import java.net.URL
import java.net.HttpURLConnection
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.IOException

case class TaskThreadFactory(prefix: String = "T", daemon: Boolean = false, priority: Int = Thread.NORM_PRIORITY) extends ThreadFactory {
  val counter = new AtomicInteger(0)

  def newThread(r: Runnable): Thread = {
    val id = counter.incrementAndGet()
    val name = prefix + "%02d".format( id )
    
    val t = new Thread(r)
    t setName name
    t setDaemon daemon
    t setPriority priority
    
    t
  }
}

object TaskManager {
  import scala.collection.mutable.Map
  
  private val pools = Map[String, ExecutorService]()
  
  val MainPoolID = "T"
  
  val MainPoolSize  = 4
  val MainPoolLimit = Integer.MAX_VALUE
      
  val ThreadKeepAliveSecs = 10L

  private def createPool(name: String, size: Int, limit: Int): ExecutorService = {
    val q = new LinkedBlockingQueue[Runnable]()
    val f = new TaskThreadFactory(name)
    new ThreadPoolExecutor(size, limit, ThreadKeepAliveSecs, TimeUnit.SECONDS, q, f)
  }
  
  private def pool(poolID: String, size: Int, limit: Int): ExecutorService = pools.synchronized {
    pools get poolID match {
      case Some(pool) => pool
      case None  => {
        val pool = createPool(poolID, size, limit)
        pools put (poolID, pool)
        pool
      }
    }
  }
  
  private def pool(task: Task[_]): ExecutorService = task match {
    case t:TaskOnSpecialPool[_] => {
      val (poolID, poolSize, poolLimit) = t.pool
      pool(poolID, poolSize, poolLimit)
    }
    case _ => pool(MainPoolID, MainPoolSize, MainPoolLimit)
  }

  
//  private val mainPool = createPool(MinPoolSize, MaxPoolSize)
//  
//  private val ioPools  = Map[String, ExecutorService]()
//  
//  def ioPool(target: String): ExecutorService = ioPools.synchronized {
//    ioPools get target match {
//      case Some(pool) => pool
//      case None => {
//        val pool = createPool(MinIOPoolSize, MaxIOPoolSize)
//        ioPools put (target, pool)
//        pool
//      }
//    }
//  }
  
//  def pool(task: Task[_], poolHint: String): ExecutorService = task match {
//    case io:IOTask[_] => pool(io.target, MinIOPoolSize, MaxIOPoolSize)
//    case _ => {
//      val poolID =
//        if (poolHint.isEmpty) MainPoolID
//        else poolHint
//      pool(poolID, MinPoolSize, MaxIOPoolSize)
//    }
//  }

  def shutdown(): Unit = {
    val allPools = pools.values.toList
    allPools.foreach( _.shutdownNow() )
    allPools.foreach( _.awaitTermination(3, TimeUnit.SECONDS) )
  }
  
  def submit[A](task: Task[A], poolHint: String): Future[A] = {
    notifyListeners( counter.incrementAndGet )
    pool(task) submit new ScheduledTask(task)
  }
  
  private val counter = new AtomicInteger()
  private var listeners = List[Int => Unit]()
  
  def registerOnChange(f: Int => Unit): Unit = listeners synchronized {
    listeners = f :: listeners
  }
  
  private def notifyListeners(count: Int): Unit = listeners synchronized {
    for (listener <- listeners)
      listener( count )
  }
  
  private class ScheduledTask[A](task: Task[A]) extends Callable[A] {
    def call(): A =
      try task.execute()
      finally notifyListeners( counter.decrementAndGet() )
  }
}

object Task {
  def create[A](code: =>A): Task[A] = new Task[A] {
    def execute(): A = { code }
  }
  
//  def wrap[A](ts: List[Task[A]]): Task[List[A]] = new Task[List[A]] {
//    def execute(): List[A] = ts.map( _.execute() )
//  }
}

trait Task[A] {
    
  // executes the task synchronously
  def execute(): A
  
  // submits the task for concurrent execution
  def submit(poolHint: String = ""): Future[A] =
    TaskManager.submit(this, poolHint)
  
  // forks all tasks for asynchronous execution,
  // then submits a special task that is joining the results and then executes the callback
//  def fork[B](ts: Seq[Task[B]], callback: List[B] => Unit): Unit = {
//    if (ts.isEmpty)
//      return ()
//      
//    // execute one task directly
//    if (ts.tail.isEmpty) {
//      callback( ts.head.execute() :: Nil )
//    
//    // execute multiple tasks asynchronously
//    // submit a special JoiningTask that joins the results, then calls back
//    } else {
//      val futs = ts.map( _.submit() )
//      new TaskJoiner(futs, callback).submit()
//    }
//  }
}

trait TaskOnSpecialPool[A] extends Task[A] {
  def pool: (String, Int, Int)
}

trait IOTask[A] extends TaskOnSpecialPool[A] {
  val MinPoolSize = 2
  val MaxPoolSize = 4
  
  final def pool = (target, MinPoolSize, MaxPoolSize)
  
  def target: String
}

//trait JoiningTask[A] {
//  
//  def forkJoin[B](ts: Seq[Task[B]]): Seq[B] = {
//    if (ts.isEmpty)
//      return Nil
//    
//    // execute one task directly
//    if (ts.tail.isEmpty) {
//      ts.head.execute() :: Nil
//    
//    // execute multiple tasks asynchronously
//    // join the results
//    } else {
//      ts.map( _.submit() ).map( _.get() )
//    }
//  }
//}

//final class TaskJoiner[A](ts: Traversable[Future[A]], callback: List[A] => Unit) extends Task[Unit] with JoiningTask[Unit] {
//  def execute(): Unit =
//    callback( ts.map( _.get() ).toList )
//}

// Task processing an HTTP-URL
trait HttpTask[A] extends IOTask[A] {
  
  // url to process
  def url: URL
  
  // IOTask
  def target: String = url.getHost
  
  // processor of the response
  def processResponse(is: InputStream): A
    
  // dont keep-alive
  val ConnectionClose = false
  
  val UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:9.0.1) Gecko/20100101 Firefox/9.0.1"
  val Referer   = "http://stackoverflow.com/questions/tagged/referer" 
     
  // if sending data, define its content-type and the function to fill the OutputStream with the data
  val RequestMethod: String = "GET"
  val RequestProperties: List[(String, String)] = Nil
  val RequestContentType: Option[String]      = None
  val RequestSendData: Option[OutputStream => Unit] = None
  val FollowRedirects = false
  
  final def execute(): A = {
    val conn: HttpURLConnection = url.openConnection() match {
      case http:HttpURLConnection => http
      case _                      => throw new Exception("HttpTask only supports HTTP connections")
    }
    
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput  true
    conn setDoOutput RequestSendData.isDefined
    
    conn setRequestMethod RequestMethod
    conn setInstanceFollowRedirects FollowRedirects
        
    if (ConnectionClose)
       conn setRequestProperty ("Connection", "Close")
    
    if (RequestContentType.isDefined)
      conn setRequestProperty ("Content-Type", RequestContentType.get);
    
    conn setRequestProperty ("User-Agent", UserAgent)
    conn setRequestProperty ("Referer",    Referer)
    
    for ((propName, propValue) <- RequestProperties)
      conn setRequestProperty (propName, propValue)
  
    var in: InputStream = null
    try {
      conn.connect()
      
      RequestSendData match {
        case Some(f) => f( conn.getOutputStream )
        case None    => // request-parameters only encoded in URL
      }
      
      val respCode = conn.getResponseCode
      
      if (respCode != HttpURLConnection.HTTP_OK)
        throw new IOException("HTTP " + respCode + ": " + conn.getResponseMessage + " {" + url + "}")
      
      in = conn.getInputStream
      val res = processResponse( in )
      
      if (ConnectionClose) {
        conn.disconnect()
      }
      
      res
    } catch {
      // http://docs.oracle.com/javase/1.5.0/docs/guide/net/http-keepalive.html
      case io:IOException => {
        cleanCloseInputStream( conn.getErrorStream )
        throw io
      }
    } finally {
      cleanCloseInputStream( in )
    }
  }
  
  def cleanCloseInputStream(is: InputStream): Unit = {
    if (is == null)
      return ()
    
    try {
      while (is.read() > 0) {}
      is.close()
    } catch {
      case e:Exception =>
    }
  }
}


// A specialization that processes XML-data located by the URL
trait XmlTask[A] extends HttpTask[A] {
  import nu.xom.Builder
  
  final def processResponse(in: InputStream): A = {
    val builder = new Builder( )
    val doc = builder build in
    
    processDocument(doc)
  }
  
  def processDocument(doc: nu.xom.Document): A
}


// A specialisation that processes HTML-data located by the URL
// (XML-malformed HTML is forced into XML/XHTML by the tagsoup-parser)
//@deprecated("Use HtmlTask using JSoup instead", "")
trait XhtmlTask[A] extends HttpTask[A] {
  import org.xml.sax.helpers.XMLReaderFactory
  import nu.xom.Builder
  import nu.xom.Document
  
  val TagsoupParser = "org.ccil.cowan.tagsoup.Parser"
  
  final def processResponse(is: InputStream): A = {
    val tagsoup = XMLReaderFactory.createXMLReader( TagsoupParser )
    val builder = new Builder( tagsoup )
    val doc = builder build is
    
    processDocument(doc)
  }
  
  def processDocument(doc: Document): A
}

// HTML parsing with JSoup
trait HtmlTask[A] extends HttpTask[A] {
  import org.jsoup.Jsoup
  import org.jsoup.nodes.Document
    
  final def processResponse(in: InputStream): A = {
    val doc = Jsoup.parse(in, null /* "UTF-8" */, url.toString) 
    processDocument(doc)
  }
  
  def processDocument(doc: Document): A
}

case class DownloadTask(from: URL, to: File) extends IOTask[(URL,File)] with Logging {
  import java.io.FileOutputStream
  import java.nio.channels.{Channels, ReadableByteChannel, FileChannel}
  import scala.util.control.Exception
  
  val logID = "DownloadTask(" + from.toExternalForm + " => " + to.getAbsolutePath + ")"
  
  def target = from.getHost
  
  def execute(): (URL, File) = {
    var is: InputStream = null
    var os: FileOutputStream = null
    
    try {
      trace("connecting ...")
      is = from.openStream()
      val ich = Channels.newChannel( is )
      
      trace("transferring ...")
      os = new FileOutputStream( to )
      val och = os.getChannel()
      och.lock()
      och.transferFrom(ich, 0, 1 << 24)
      
      info("done")
      (from, to)
    
    } finally {
      try is.close() catch { case _: Throwable => }
      try os.close() catch { case _: Throwable => }
    }
  }
}