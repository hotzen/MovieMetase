package moviemetase

import java.util.concurrent._
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.net.HttpURLConnection
import scala.swing.Publisher
import java.io.File
import java.io.IOException

case class TaskThreadFactory(prefix: String, daemon: Boolean = false, priority: Int = Thread.NORM_PRIORITY) extends ThreadFactory {
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
//  private val CorePoolSize  = 8
//  private val MaxPoolSize   = 16
//  private val KeepAliveTime = 60L
//  private val KeepAliveUnit = TimeUnit.SECONDS
//  private val Queue         = new SynchronousQueue[Runnable]()
//  
//  private val RejectedExecHandler = new AbortPolicy()
//  
//  private val ThreadFactory = new ThreadFactory {
//    val counter = new AtomicInteger(0)
//
//    def newThread(r: Runnable): Thread = {
//      val t = new Thread(r)
//      val id = counter.incrementAndGet()
//      t.setName( "T" + "%02d".format( id ) )
//      t.setDaemon( false )
//      t.setPriority( Thread.NORM_PRIORITY )
//      t
//    }
//  }
//  
//  private val executor: ExecutorService =
//    new ThreadPoolExecutor(
//      CorePoolSize, MaxPoolSize,
//      KeepAliveTime, KeepAliveUnit,
//      Queue, ThreadFactory, 
//      RejectedExecHandler
//    )
  
  val MaxWorkerThreads = 4
  val MaxIOThreads = 16
  val MaxThreadsPerIOTarget = 4

  val workerPool = Executors.newFixedThreadPool(MaxWorkerThreads, TaskThreadFactory("WORK_") )
  val ioPools    = scala.collection.mutable.Map[String, ExecutorService]()
  //val ioPools    = Executors.newFixedThreadPool(MaxIOThreads)
  //val ioSemaphores = scala.collection.mutable.Map[String, ExecutorService]()
  val joinerPool = Executors.newCachedThreadPool( TaskThreadFactory("JOIN_") )
  
  def ioPool(target: String): ExecutorService = ioPools.synchronized {
    ioPools get target match {
      case Some(pool) => pool
      case None => {
        val pool = Executors.newFixedThreadPool(MaxThreadsPerIOTarget, TaskThreadFactory("IO_" + target + "_"))
        ioPools put (target, pool)
        pool
      }
    }
  }
  
  def shutdown(): Unit = {
    workerPool.shutdownNow()
    workerPool.awaitTermination(3, TimeUnit.SECONDS)
    
    for ( (target, pool) <- ioPools) {
      pool.shutdownNow()
      pool.awaitTermination(3, TimeUnit.SECONDS)
    }
    
    joinerPool.shutdownNow()
    joinerPool.awaitTermination(3, TimeUnit.SECONDS)
  }
  
  private def publishMoreTasks(): Unit = {
    val cnt = counter.incrementAndGet()
    ui.UI.publish(progress)( ActiveTasks(cnt) )
  }

  private def publishLessTasks(): Unit = {
    val cnt = counter.decrementAndGet()
    ui.UI.publish(progress)( ActiveTasks(cnt) )
  }
    
  def submit[A](task: Task[A]): Future[A] = task match {
    case x:JoiningTask[_] =>
      joinerPool submit task

    case io:IOTask =>
      ioPool( io.target ) submit task

    case _ => {
      publishMoreTasks()
      workerPool submit new CompletionCallbackTask(task)
    }
  }
        
  private val counter = new AtomicInteger(0)
  val progress = new Publisher { }
  
  case class ActiveTasks(tasks: Int) extends scala.swing.event.Event
  
  private class CompletionCallbackTask[A](task: Task[A]) extends Callable[A] {
    def call(): A =
      try     task.execute()
      finally publishLessTasks()
  }
}

object Task {
  def create[A](code: =>A): Task[A] = new Task[A] {
    def execute(): A = { code }
  }
  
  def createSeq[A](ts: List[Task[A]]): Task[List[A]] = new Task[List[A]] {
    def execute(): List[A] = ts.map( _.execute() )
  }
}

trait Task[A] extends Callable[A] {
    
  // executes the task synchronously
  def execute(): A
  
  // Callable[A]
  final def call(): A = execute()
  
  // submits the task for concurrent execution
  def submit(): Future[A] = TaskManager submit this
 
  // forks all tasks for asynchronous execution,
  // then submits a special task that is joining the results and then executes the callback
  def fork[B](ts: Seq[Task[B]], callback: List[B] => Unit): Unit = {
    if (ts.isEmpty)
      return ()
      
    // execute one task directly
    if (ts.tail.isEmpty) {
      callback( ts.head.execute() :: Nil )
    
    // execute multiple tasks asynchronously
    // submit a special JoiningTask that joins the results, then calls back
    } else {
      val futs = ts.map( _.submit() )
      new TaskJoiner(futs, callback).submit()
    }
  }
}

trait IOTask {
  def target: String
}

trait JoiningTask[A] { self: Task[A] =>
  
  def forkJoin[B](ts: Seq[Task[B]]): Seq[B] = {
    if (ts.isEmpty)
      return Nil
    
    // execute one task directly
    if (ts.tail.isEmpty) {
      ts.head.execute() :: Nil
    
    // execute multiple tasks asynchronously
    // join the results
    } else {
      ts.map( _.submit() ).map( _.get() )
    }
  }
}

final class TaskJoiner[A](ts: Traversable[Future[A]], callback: List[A] => Unit) extends Task[Unit] with JoiningTask[Unit] {
  def execute(): Unit =
    callback( ts.map( _.get() ).toList )
}

// Task processing an HTTP-URL
trait HttpTask[A] extends Task[A] with IOTask {
  
  // url to process
  def url: URL
  
  // IOTask
  def target: String = url.getHost
  
  // processor of the response
  def process(is: InputStream): A
    
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
      val res = process( in )
      
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


// A specialisation that processes XML-data located by the URL
trait XmlTask[A] extends HttpTask[A] {
  import nu.xom.Builder
  
  final def process(in: InputStream): A = {
    val builder = new Builder( )
    val doc = builder build in
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): A
}


// A specialisation that processes HTML-data located by the URL
// (XML-malformed HTML is forced into XML/XHTML by the tagsoup-parser)

@deprecated("Use HtmlTask using JSoup instead", "")
trait XhtmlTask[A] extends HttpTask[A] {
  import org.xml.sax.helpers.XMLReaderFactory
  import nu.xom.Builder
  import nu.xom.Document
  
  val TagsoupParser = "org.ccil.cowan.tagsoup.Parser"
  
  final def process(is: InputStream): A = {
    val tagsoup = XMLReaderFactory.createXMLReader( TagsoupParser )
    val builder = new Builder( tagsoup )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: Document): A
}

trait HtmlTask[A] extends HttpTask[A] {
  import org.jsoup.Jsoup
  import org.jsoup.nodes.Document
    
  final def process(in: InputStream): A = {
    val doc = Jsoup.parse(in, null /* "UTF-8" */, url.toString) 
    process(doc)
  }
  
  def process(doc: Document): A
}

case class DownloadTask(from: URL, to: File) extends Task[(URL,File)] with Logging {
  import java.io.FileOutputStream
  import java.nio.channels.{Channels, ReadableByteChannel, FileChannel}
  import scala.util.control.Exception
  
  val logID = "DownloadTask(" + from.toExternalForm + " => " + to.getAbsolutePath + ")"
  
  def execute(): (URL, File) = {
    var is: InputStream = null
    var os: FileOutputStream = null
    
    try {
      trace("connecting ...")
      is = from.openStream()
      val ich = Channels.newChannel( is )
      
      trace("opening & locking target-file ...")
      os = new FileOutputStream( to )
      val och = os.getChannel()
      och.lock()
      
      trace("transferring ...")
      och.transferFrom(ich, 0, 1 << 24)
      
      info("successfully downloaded")
      (from, to)
    
    } finally {
      try is.close() catch { case _ => }
      try os.close() catch { case _ => }
    }
  }
}