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
import java.util.concurrent.atomic.AtomicReference

case class TaskThreadFactory(prefix: String = "T", daemon: Boolean = false, priority: Int = Thread.NORM_PRIORITY) extends ThreadFactory {
  val stats = new AtomicInteger(0)

  def newThread(r: Runnable): Thread = {
    val id = stats.incrementAndGet()
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
  
  val MainPoolID    = "M"
  val MainPoolSize  = 4
  val MainPoolLimit = Integer.MAX_VALUE

  val ThreadKeepAliveSecs = 10L

  private def createPool(name: String, size: Int, limit: Int): ExecutorService = {
    val q = new LinkedBlockingQueue[Runnable]()
    val f = new TaskThreadFactory(name)
    new ThreadPoolExecutor(size, limit, ThreadKeepAliveSecs, TimeUnit.SECONDS, q, f)
  }
  
  def pool(id: String, size: Int, limit: Int): ExecutorService = pools.synchronized {
    //println("TaskManager.pool" + (id, size, limit))
    pools get id match {
      case Some(pool) => pool
      case None  => {
        val pool = createPool(id, size, limit)
        pools put (id, pool)
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
    notifyListeners( moreTasks() )
    try {
      getPoolFor(task) submit new ScheduledTask(task)

    } catch { case t:Throwable => {
      error("could not submit task: " + t.getMessage)
      
      notifyListeners( lessTasks() )
      new ErrorJFuture(t)
    }}
  }
  
  case class TaskStats(active: Int, total: Int) {
    def completed: Int = total - active
    def completedRatio: Float = completed.toFloat / total
    def completedPercent: Int = (completedRatio * 100).toInt
  }
  private val stats = new AtomicReference[TaskStats]( TaskStats(0,0) )
  private var listeners = List[TaskStats => Unit]()
  
  private def moreTasks(): TaskStats = {
    while (true) {
      val cur = stats.get
      val upd = TaskStats(cur.active + 1, cur.total + 1)
      if (stats.compareAndSet(cur, upd))
        return upd
    }
    null
  }
  
  private def lessTasks(): TaskStats = {
    while (true) {
      val cur = stats.get
      val upd =
        if (cur.active > 1)
          TaskStats(cur.active - 1, cur.total)
        else
          TaskStats(0, 0)

      if (stats.compareAndSet(cur, upd))
        return upd
    }
    null
  }
    
    
  def registerOnChange(f: TaskStats => Unit): Unit = listeners synchronized {
    listeners = f :: listeners
  }
  
  private def notifyListeners(active: TaskStats): Unit = listeners synchronized {
    for (listener <- listeners)
      listener( active )
  }
  
  private class ScheduledTask[A](task: Task[A]) extends Callable[A] {
    def call(): A =
      try task.execute()
      finally notifyListeners( lessTasks() )
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
    
  // submit for asynchronous processing using the new Scala Futures
  // http://docs.scala-lang.org/sips/pending/futures-promises.html
  private def asyncExecContext = ExecutionContext.fromExecutorService( TaskManager getPoolFor this )   
  def async(): Future[A] = Future({ execute() })(asyncExecContext)
}

trait DedicatedPool {
  def pool: (String, Int, Int) // poolID, min threads, max threads
}

object HumanTasks extends scala.swing.Publisher

trait HumanTaskEvent[A] extends scala.swing.event.Event {
  def reply(a: A): Unit //XXX better name?
}

trait HumanTask[A] extends Task[A] with DedicatedPool {
  import scala.concurrent.SyncVar
  
  val MinPoolSize = 1
  val MaxPoolSize = Int.MaxValue
  def pool = ("H", MinPoolSize, MaxPoolSize)
  
  def createEvent(cb: A => Unit): HumanTaskEvent[A]
  
  final def execute(): A = {
    val v = new SyncVar[A]
    HumanTasks synchronized {
      HumanTasks publish createEvent(a => v put a)
    }
    v.get
  }
}

//case class AskUserEvent(question: String, cb: String => Unit) extends HumanTaskEvent[String] {
//  def reply(a: String): Unit = cb(a)
//}
//
//case class AskUserTask(question: String) extends HumanTask[String] {
//  def createEvent(cb: String => Unit): HumanTaskEvent[String] = AskUserEvent(question, cb)
//}
//
//... swing ...
//listenTo(HumanTask)
//reactions += {
//  case AskUserEvent(question, cb) => {
//    val answer = showDialog(q)
//    cb(answer)
//  }
//}

trait IOTask[A] extends Task[A] with DedicatedPool {
  val MinPoolSize = 2
  val MaxPoolSize = 4
  
  def pool = (target, MinPoolSize, MaxPoolSize)
  
  // I/O-target, e.g. domain-name "s3.amazonaws.com"
  def target: String
}

object IOTask {
//  def getTargetByURL(url: URL): String = {
//    val host = url.getHost
//    val hostParts = host.split("\\.") // regex
//    
//    if (hostParts.length < 2)
//      return host
//    
//    val hostPartsRev = hostParts.reverse
//    hostPartsRev.tail.head + "." + hostPartsRev.head
//  }
}

case class HttpRedirectException(code: Int, message: String, location: String, url: URL) extends IOException {
  override def getMessage(): String =
    "HTTP " + code + ": " + message + " REDIRECT to " + location + " {" + url + "}"
}

case class HttpResponseException(code: Int, message: String, url: URL) extends IOException {
  override def getMessage(): String =
    "HTTP " + code + ": " + message + " {" + url + "}"
}


object HttpTask {
  //import scala.collection.concurrent.Map
  import java.util.concurrent.ConcurrentHashMap
  
  case class HttpCookie(name: String, value: String, attrs: List[String] = Nil) {
    def toHeader(): String = name+"="+value
  }
    
  

  
  //val CookieStore = new ConcurrentHashMap[String, List[HttpCookie]]()
  val CookieStore = new scala.collection.mutable.HashMap[String, List[HttpCookie]]()
  
  //case class HostCookies(host: String, cookies: List[HttpCookie])
  //val CookieStore = new ConcurrentHashMap[HostCookies]()
}

// Task processing an HTTP-URL
trait HttpTask[A] extends IOTask[A] {
  import HttpTask._
  import Util.urlUtils
  
  // url to process
  def url: URL

  // IOTask
  def target: String = url.domainName
    
  // optional "raw"-processor
  def preProcess(conn: HttpURLConnection, headers: Map[String, List[String]]): Unit = {}
  
  // processor of the response
  def processResponse(is: InputStream): A

  var CloseConnection = false
  var Caching = true
  var FollowRedirects = false
  var StoreCookies = false
  
  var RequestMethod: String = "GET"
  var RequestHeaders = ListBuffer[(String, String)]()
  
  // POSTing data ...
  var RequestContentType: Option[String] = None
  var RequestData: Option[OutputStream => Unit] = None
    
  // basic browser-mimicry
  var UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:14.0) Gecko/20100101 Firefox/14.0.1"
  var Referer   = "http://stackoverflow.com/questions/tagged/referer" 
    
  def setup(conn: HttpURLConnection): Unit = {
      
    conn setUseCaches Caching
    conn setAllowUserInteraction false
    conn setDoInput true
    conn setDoOutput RequestData.isDefined
    
    conn setRequestMethod RequestMethod
    conn setInstanceFollowRedirects FollowRedirects
        
    if (CloseConnection)
       conn setRequestProperty ("Connection", "Close")
    
    for (ct <- RequestContentType)
      conn setRequestProperty ("Content-Type", ct)
    
    conn setRequestProperty ("User-Agent", UserAgent)
    conn setRequestProperty ("Referer",    Referer)
    
    if (StoreCookies) {
      CookieStore synchronized {
        val domain = cookieDomain( conn.getURL )
        
        for (cookies <- CookieStore get domain) {
          val header = cookies.map(_.toHeader).mkString(";")
          conn setRequestProperty ("Cookie", header)
        }
      }
    }
    
    for ((name, value) <- RequestHeaders)
      conn setRequestProperty (name, value)
  }
    
  def connect(conn: HttpURLConnection): InputStream = {
    conn.connect()
      
    for (f <- RequestData)
      f( conn.getOutputStream )

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
    if (conn != null && CloseConnection) {
      conn.disconnect()
    }
  }
  
  final def execute(): A = {
    val conn: HttpURLConnection = url.openConnection() match {
      case c:HttpURLConnection => c
      case _                   => throw new IllegalArgumentException("HttpTask only supports HTTP connections")
    }
    
    try {
      setup(conn)
      val is = connect( conn )
      try {
        val hdrs = headers(conn)
        storeCookies(conn, hdrs)
        preProcess(conn, hdrs)
        val res = processResponse( is )
        disconnect( conn )
        res
      } finally cleanCloseInputStream( is )
    } catch { case e:IOException => {
        cleanCloseInputStream( conn.getErrorStream )
        throw e
    }} finally disconnect( conn )
  }
  
  def onRedirect(code: Int, loc: String, conn: HttpURLConnection): HttpURLConnection = {
    throw new HttpRedirectException(code, conn.getResponseMessage, loc, conn.getURL)
  }
  
  def onError(code: Int, conn: HttpURLConnection): InputStream = {
    throw new HttpResponseException(code, conn.getResponseMessage, conn.getURL)
  }
  
  def headers(conn: HttpURLConnection): Map[String, List[String]] = {
    for ( (name, values) <- scala.collection.convert.Wrappers.JMapWrapper( conn.getHeaderFields ))
      yield (name -> scala.collection.convert.Wrappers.JListWrapper(values).toList)
  }.toMap
  
    
  def storeCookies(conn: HttpURLConnection, headers: Map[String, List[String]]): Unit = {
    if (StoreCookies) {
      val domain = cookieDomain( conn.getURL )
      
      val newCookies: List[HttpCookie] = 
        headers.get("Set-Cookie").getOrElse( Nil ).
          map( str => str.split(";") ).
          flatMap( parts => {
            val pos = parts.head.indexOf("=")
            if (pos > 0) {
              val name  = parts.head.slice(0, pos)
              val value = parts.head.slice(pos+1, parts.head.length)
              Some( HttpCookie(name, value, parts.tail.toList) )
            } else None
          }).distinct
        
      val newCookieNames: Set[String] = newCookies.map(_.name).toSet
            
      CookieStore.synchronized {
        val keep = CookieStore.get(domain) match {
          case Some(cs) => cs.filter(c => !newCookieNames.contains(c.name) )
          case None     => Nil
        }
        CookieStore.put(domain, newCookies ::: keep)
      }
    }
  }
  
  import Util.urlUtils
  def cookieDomain(url: URL): String = url.domainName

  // http://docs.oracle.com/javase/1.5.0/docs/guide/net/http-keepalive.html
  def cleanCloseInputStream(is: InputStream) {
    if (is != null) {
      try {
        while (is.read() > 0) {}
        is.close()
      } catch {
        case e:IOException =>
      }
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