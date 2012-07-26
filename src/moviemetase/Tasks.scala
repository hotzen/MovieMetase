package moviemetase

import java.net.URL
import java.net.HttpURLConnection
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.IOException
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.concurrent.{Future => JFuture}
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadFactory
import java.util.concurrent.ExecutorService
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import scala.concurrent.future
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

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
  
  def pool(poolID: String, size: Int, limit: Int): ExecutorService = pools.synchronized {
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
  
  def getPoolFor(task: Task[_]): ExecutorService = task match {
    case p:DedicatedPool => {
      val (poolID, poolSize, poolLimit) = p.pool
      pool(poolID, poolSize, poolLimit)
    }
    case _ =>
      pool(MainPoolID, MainPoolSize, MainPoolLimit)
  }

  def shutdown(): Unit = {
    val allPools = pools.values.toList
    allPools.foreach( _.shutdownNow() )
    allPools.foreach( _.awaitTermination(3, TimeUnit.SECONDS) )
  }
  
  def submit[A](task: Task[A]): JFuture[A] = {
    notifyListeners( counter.incrementAndGet )
    try {
      getPoolFor(task) submit new ScheduledTask(task)

    } catch { case t:Throwable => {
      error("could not submit task: " + t.getMessage)
      
      notifyListeners( counter.decrementAndGet() )
      new ErrorJFuture(t)
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
  
  private class ErrorJFuture[A](t: Throwable) extends JFuture[A] {
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
}

trait Task[A] {
  
  def execute(): A
  
  // submits the task for concurrent execution
  def submit(): JFuture[A] = TaskManager submit this
    
  // submit asynchronously using the new Scala Futures
  // http://docs.scala-lang.org/sips/pending/futures-promises.html
  private def asyncExecContext = ExecutionContext.fromExecutorService( TaskManager getPoolFor this )   
  def async(): Future[A] = Future({ execute() })(asyncExecContext)
}

trait DedicatedPool {
  def pool: (String, Int, Int) // poolID, min threads, max threads
}

trait IOTask[A] extends Task[A] with DedicatedPool {
  val MinPoolSize = 2
  val MaxPoolSize = 4
  
  def pool = (target, MinPoolSize, MaxPoolSize)
  
  // I/O-target, e.g. domain-name "s3.amazonaws.com"
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

case class HttpRedirectException(code: Int, message: String, location: String, url: URL) extends Exception {
  override def getMessage(): String =
    "HTTP " + code + ": " + message + " REDIRECT to " + location + " {" + url + "}"
}

case class HttpResponseException(code: Int, message: String, url: URL) extends Exception {
  override def getMessage(): String =
    "HTTP " + code + ": " + message + " {" + url + "}"
}

object HttpTask {
  //import scala.collection.concurrent.Map
  import java.util.concurrent.ConcurrentHashMap
  
  case class HttpCookie(name: String, value: String, extra: String = "")
  
  val CookieStore = new ConcurrentHashMap[String, List[HttpCookie]]()
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
      
  // if sending data, define its content-type and the function to fill the OutputStream with the data
  var RequestMethod: String = "GET"
  var RequestProperties = ListBuffer[(String, String)]()
  var RequestContentType: Option[String] = None
  var RequestData: Option[OutputStream => Unit] = None
  
  var FollowRedirects = false
  var StoreCookies = false
  var cookies = List[(String, String)]()
  
  def setup(url: URL): HttpURLConnection = {
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
    
  def connect(conn: HttpURLConnection): InputStream = {
    conn.connect()
    
    RequestData match {
      case Some(f) => f( conn.getOutputStream )
      case None    => 
    }
    
    if (StoreCookies) {
      val headers = scala.collection.convert.Wrappers.JMapWrapper( conn.getHeaderFields )
      // TODO ...
    }

    val respCode = conn.getResponseCode
    
    // redirect
    if (respCode >= 300 && respCode < 400) {
      if (!conn.getInstanceFollowRedirects) {
        val newLoc  = conn getHeaderField "Location"
        val newConn = onRedirect(respCode, newLoc, conn)
        return connect( newConn )
      }
    // errors
    } else if (respCode != HttpURLConnection.HTTP_OK) {
      return onError(respCode, conn)
    }

    conn.getInputStream
  }
    
  def disconnect(conn: HttpURLConnection) {
    if (conn != null && ConnectionClose) {
      conn.disconnect()
    }
  }
  
  final def execute(): A = {
    val conn = setup( url )
    try {
      val is = connect( conn )
      try {
        val res = processResponse( is )
        disconnect( conn )
        res
      } finally cleanCloseInputStream( is )
    
    } catch { case t:Throwable => {
        cleanCloseInputStream( conn.getErrorStream )
        throw t
    
    }} finally disconnect( conn )
  }
  
  def onRedirect(code: Int, loc: String, conn: HttpURLConnection): HttpURLConnection = {
    throw new HttpRedirectException(code, conn.getResponseMessage, loc, conn.getURL)
  }
  
  def onError(code: Int, conn: HttpURLConnection): InputStream = {
    throw new HttpResponseException(code, conn.getResponseMessage, conn.getURL)
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