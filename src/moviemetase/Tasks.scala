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

object TaskManager {
  private val CorePoolSize  = 4
  private val MaxPoolSize   = Integer.MAX_VALUE
  private val KeepAliveTime = 60L
  private val KeepAliveUnit = TimeUnit.SECONDS
  private val Queue         = new SynchronousQueue[Runnable]()
  
  private val RejectedExecHandler = new AbortPolicy()
  
  private val ThreadFactory = new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r)
      t.setName( "T" + "%02d".format( counter.incrementAndGet() ) )
            
      t.setDaemon( false )
      t.setPriority( Thread.NORM_PRIORITY )
      t
    }
  }
  
  private val executor: ExecutorService =
    new ThreadPoolExecutor(
      CorePoolSize, MaxPoolSize,
      KeepAliveTime, KeepAliveUnit,
      Queue, ThreadFactory, 
      RejectedExecHandler
    )
     
  def shutdown(): Unit = {
    executor.shutdownNow()
    executor.awaitTermination(3, TimeUnit.SECONDS)
  }
  
  private def publishMoreTasks(): Unit = {
    val cnt = counter.incrementAndGet()
    ui.UI.publish(progress)( ActiveTasks(cnt) )
  }

  private def publishLessTasks(): Unit = {
    val cnt = counter.decrementAndGet()
    ui.UI.publish(progress)( ActiveTasks(cnt) )
  }
        
  def submit[A](task: Task[A]): Future[A] = {
    publishMoreTasks()
    executor submit new CompletionPublisher(task)
  }
        
  private val counter = new AtomicInteger(0)
  val progress = new Publisher { }
  
  case class ActiveTasks(tasks: Int) extends scala.swing.event.Event
  
  private class CompletionPublisher[A](task: Task[A]) extends Callable[A] {
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
  final def submit(): Future[A] = TaskManager submit this
  
  // create a new task that executes <this> and then <next>
  final def then[B](next: Task[B]): Task[B] = new SerialTask[B](this, next)
  
  // create a new task that executes <this> and then forks the tasks produced by <f>
  final def thenFork[B](f: A => List[Task[B]]): Task[List[B]] = new ForkingTask[A,B](this, f)
}

class SerialTask[B](val t1: Task[_], val t2: Task[B]) extends Task[B] {
  final def execute(): B = {
    t1.execute()
    t2.execute()
  }
}

class ForkingTask[A,B](val t: Task[A], val f: A => List[Task[B]]) extends Task[List[B]] {
  final def execute(): List[B] = {
     
    val res: A = t.execute()
    
    // new tasks
    val ts = f( res )
    if (ts.isEmpty)
      return Nil
    
    // fork/join
    val futs = ts.tail.map( _.submit() )
    val res2 = ts.head.execute()
    res2 :: futs.map( _.get() )
  }
}


// Task processing an HTTP-URL
trait HttpTask[A] extends Task[A] {
  
  // url to process
  def url: URL
  
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
    
    conn.connect()
    
    RequestSendData match {
      case Some(f) => f( conn.getOutputStream )
      case None    => // request-parameters only encoded in URL
    }

    var in: InputStream = null
    try {
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
  
  def cleanCloseInputStream(is: InputStream): Unit =
    try {
      while (is.read() > 0) {}
      is.close()
    } catch { case e:Exception => }
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
  
  val TagsoupParser = "org.ccil.cowan.tagsoup.Parser"
  
  final def process(is: InputStream): A = {
    val tagsoup = XMLReaderFactory.createXMLReader( TagsoupParser )
    val builder = new Builder( tagsoup )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): A
}

trait HtmlTask[A] extends HttpTask[A] {
  import org.jsoup.Jsoup
    
  final def process(in: InputStream): A = {
    val doc = Jsoup.parse(in, null /* "UTF-8" */, url.toString) 
    process(doc)
  }
  
  def process(doc: org.jsoup.nodes.Document): A
}

case class DownloadTask(from: URL, to: File) extends Task[(URL,File)] with Logging {
  import java.io.FileOutputStream
  import java.nio.channels.{Channels, ReadableByteChannel, FileChannel}
  import scala.util.control.Exception
  
  val logID = "DownloadTask(" + from.toExternalForm + " => " + to + ")"
  
  def execute(): (URL, File) = {
    var is: InputStream = null
    var os: FileOutputStream = null
    
    try {
      trace("connecting ...")
      is = from.openStream()
      val ich = Channels.newChannel( is )
      
      trace("opening target-file ...")
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