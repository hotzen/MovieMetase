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
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicLong
import scala.util.Random

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

object TaskManager extends Logging {
  import scala.collection.mutable.Map
  
  val logID = "TaskManager"
  
  private val pools = Map[String, ExecutorService]()
  
  val MainPoolID = "M"
  
  val MainPoolSize  = 4
  val MainPoolLimit = Integer.MAX_VALUE
      
  val ThreadKeepAliveSecs = 10L

  private def createPool(name: String, size: Int, limit: Int): ExecutorService = {
    val q = new LinkedBlockingQueue[Runnable]()
    val f = new TaskThreadFactory(name)
    new ThreadPoolExecutor(size, limit, ThreadKeepAliveSecs, TimeUnit.SECONDS, q, f)
  }
  
  private def pool(poolID: String, size: Int, limit: Int): ExecutorService = pools.synchronized {
    println("TaskManager.pool" + (poolID, size, limit))
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
    try {
      pool(task) submit new ScheduledTask(task)

    } catch { case t:Throwable => {
      error("could not submit task: " + t.getMessage)
      
      notifyListeners( counter.decrementAndGet() )
      new ErrorFuture(t)
    }}
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
  
  private class ErrorFuture[A](t: Throwable) extends Future[A] {
    def get(): A = throw t
    def get(timeout: Long, unit: TimeUnit) = get()
    
    def cancel(interrupt: Boolean): Boolean = false
    def isCancelled(): Boolean = false
    def isDone(): Boolean = true
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
  
  def pool = (target, MinPoolSize, MaxPoolSize)
  
  def target: String
}

object IOTask {
  def getTargetByURL(url: URL): String = {
    val host = url.getHost
    val hostParts = host.split("\\.") // regex
    
    if (hostParts.length < 2)
      return host
    
    val hostPartsRev = hostParts.reverse
    hostParts.tail.head + "." + hostParts.head
  }
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

case class HttpResponseCodeException(code: Int, message: String, url: URL) extends Exception {
  override def getMessage(): String =
    "HTTP " + code + ": " + message + " {" + url + "}"
}

// Task processing an HTTP-URL
trait HttpTask[A] extends IOTask[A] {
  
  // url to process
  def url: URL

  // IOTask
  def target: String = IOTask.getTargetByURL(url)
    
  // processor of the response
  def processResponse(is: InputStream): A

  var ConnectionClose = false
  
  var UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:14.0) Gecko/20100101 Firefox/14.0.1"
  var Referer   = "http://stackoverflow.com/questions/tagged/referer" 
  var Throttle: Option[(Int, Int)] = None
    
  // if sending data, define its content-type and the function to fill the OutputStream with the data
  var RequestMethod: String = "GET"
  var RequestProperties = ListBuffer[(String, String)]()
  var RequestContentType: Option[String] = None
  var RequestData: Option[OutputStream => Unit] = None
  
  var FollowRedirects = false
  var StoreCookies = false
  var cookies = List[(String, String)]()
  
  def prepare(url: URL): HttpURLConnection = {
    val conn: HttpURLConnection = url.openConnection() match {
      case http:HttpURLConnection => http
      case _                      => throw new Exception("HttpTask only supports HTTP connections")
    }
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput  true
    conn setDoOutput RequestData.isDefined
    
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
    
    conn
  }
  
  private var sleepRandomizer = new Random
  private var sleepLast: Long = 0
    
  def sleep(min: Int, max: Int): Unit = {
    val t = min + sleepRandomizer.nextInt( max - min )
    //this.asInstanceOf[Logging].trace("Throttle active: sleeping for %.3f secs".format(t/1000))
    println("Throttle active: sleeping for %.3f secs".format(t.toFloat/1000))
    Thread sleep t.toLong
    
    sleepLast = System.currentTimeMillis
  }
  
  def connect(conn: HttpURLConnection): InputStream = {
    Throttle match {
      case Some((min, max)) => sleep(min, max)
      case None =>
    }
    conn.connect()
    
    RequestData match {
      case Some(f) => f( conn.getOutputStream )
      case None    => 
    }
    
    if (StoreCookies) {
//      val headers = scala.collection.convert.Wrappers.JMapWrapper( conn.getHeaderFields )
//      val newCookies =
//        headers.get("Set-Cookie").flatMap( jlist => scala.collection.convert.Wrappers.JListWrapper(jlist).toList ).
//          map( foo => foo)
//        
//        
//        for (cookieDefs <- ;
//             cookieDef  <- 
//          yield cookieDef
//        
//        .map()
//        
//        headers.keys.filter( _.startsWith("Set-Cookie") ).
//          flatMap( headers.get(_) ).map( foo => foo )
//        
//        for (header     <- headers.keys if header.startsWith("Set-Cookie");
//             cookieDefs <- headers.get(header);
//             cookieDef  <-  )
//          yield cookieDef //cookieDef.split(";")(0)
//     
//      
//        val cookieDef = headers.get( header ).get
//        val cookie    = cookieDef.split(";")(0)
//        
//        
//          GDSESS=ID=67231603decc62df:TM=1342985505:C=c:IP=92.231.163.3-:S=ADSvE-eEvfNHYVsS43d3v5aMRnXdjZz9_w; path=/; domain=google.com; expires=Sun, 22-Jul-2012 22:31:45 GMT
    }

    val respCode = conn.getResponseCode
    
    // redirect
    if (respCode >= 300 && respCode < 400) {
      if (!conn.getInstanceFollowRedirects) {
        val newLoc  = conn getHeaderField "Location"
        val newConn = onRedirect(respCode, newLoc, conn)

        UserAgent = "Lynx/2.8.7dev.9 libwww-FM/2.14"
        Referer   = conn.getURL.toString
        return connect( newConn )
      }
    }
    // non-200er
    else if (respCode != HttpURLConnection.HTTP_OK) {
      throw new HttpResponseCodeException(respCode, conn.getResponseMessage, conn.getURL)
    }

    conn.getInputStream
  }
  
  def onRedirect(code: Int, loc: String, conn: HttpURLConnection): HttpURLConnection = {
    throw new Exception("Unhandled Redirect to " + loc)
  }
  
  def disconnect(conn: HttpURLConnection) {
    if (conn != null && ConnectionClose) {
      conn.disconnect()
    }
  }
  
  final def execute(): A = {
    val conn = prepare( url )
    try {
      val is = connect( conn )
      try {
        val res = processResponse( is )
        disconnect( conn )
        res
      } finally
          cleanCloseInputStream( is )
    
    } catch { case t:Throwable => {
        cleanCloseInputStream( conn.getErrorStream )
        throw t

    }} finally
         disconnect( conn )
  }
  
  // http://docs.oracle.com/javase/1.5.0/docs/guide/net/http-keepalive.html
  def cleanCloseInputStream(is: InputStream) {
    if (is != null) {
      try {
        while (is.read() > 0) {}
        is.close()
      } catch { case e:Exception => }
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