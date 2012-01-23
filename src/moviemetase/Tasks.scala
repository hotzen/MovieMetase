package moviemetase

import java.util.concurrent._
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.net.HttpURLConnection
import java.util.WeakHashMap
import java.lang.ref.WeakReference
import scala.swing.Publisher
import java.io.File

object TaskManager {
  private val CorePoolSize  = 4
  private val MaxPoolSize   = Integer.MAX_VALUE
  private val KeepAliveTime = 60L
  private val KeepAliveUnit = TimeUnit.SECONDS
  private val Queue         = new SynchronousQueue[Runnable]()
  
  private val RejectedExecHandler = new AbortPolicy()
  
  private val ThreadFactory = new ThreadFactory {
    val counter = new AtomicInteger(1)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r)
      t.setName( "T" + counter.getAndIncrement() )
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
  
  private def publishMoreTasks(): Unit = 
    ui.UI.publish(progress)( ActiveTasks(counter.incrementAndGet()) )
  
  private def publishLessTasks(): Unit =
    ui.UI.publish(progress)( ActiveTasks(counter.decrementAndGet()) )
      
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
  def create[A](code: => A): Task[A] = new Task[A] {
    def execute(): A = { code }
  }
  
  def createSeq[A](ts: List[Task[A]]): Task[List[A]] = new Task[List[A]] {
    def execute(): List[A] = ts.map(_.execute())
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

//case class TaskInfo(weakFuture: WeakReference[Future[_]]) {
//  def future: Option[Future[_]] = {
//    val ref = weakFuture.get()
//    if (ref == null)
//      None
//    else
//      Some(ref)
//  }
//}


class SerialTask[B](val t1: Task[_], val t2: Task[B]) extends Task[B] {
  final def execute(): B = {
    t1.execute()
    t2.execute()
  }
}

class ForkingTask[A,B](val t: Task[A], val f: A => List[Task[B]]) extends Task[List[B]] {
  final def execute(): List[B] = {
    
    // execute task 
    val res = t.execute()
    
    // calculate tasks to be forked
    val ts = f( res )
    
    if (ts.isEmpty)
      return Nil
    
    // first task and rest of tasks
    val t1 = ts.head
    val tt = ts.tail
    
    // submit rest of tasks
    val ft = tt.map( _.submit() )
    
    // execute first task
    val r1 = t1.execute()
    
    // join rest tasks
    val rt = ft.map( _.get() )
    
    r1 :: rt
  }
}


// Task processing an URL
trait UrlTask[A] extends Task[A] {
  
  // url to process
  def url: URL
  
  // processor of the response
  def process(is: InputStream): A
    
  // dont keep-alive
  val ConnectionClose = true
  
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
      case _                      => throw new Exception("UrlTask only supports HTTP connections")
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
    
    val respCode = conn.getResponseCode
    if (respCode != HttpURLConnection.HTTP_OK)
      throw new Exception("HTTP " + respCode + ": " + conn.getResponseMessage + " {" + url + "}")
        
    val in = conn.getInputStream
    
    try {
      val res = process( in )
      
      if (!ConnectionClose)
        conn.disconnect()
        
      res
    } catch {
      case e:Exception => {
        System.err.println("UrlProcessor failed! URL: " + url)
        e.printStackTrace( System.err )
        throw e
      }
    } finally {
      in.close()
    }
  }
}


// A specialisation that processes XML-data located by the URL
trait XmlTask[A] extends UrlTask[A] {
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
trait XhtmlTask[A] extends UrlTask[A] {
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

trait HtmlTask[A] extends UrlTask[A] {
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