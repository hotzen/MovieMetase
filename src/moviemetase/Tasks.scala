package moviemetase

import java.util.concurrent._
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.net.URLConnection

object TaskExecutor {
  val CorePoolSize  = 4
  val MaxPoolSize   = Integer.MAX_VALUE
  val KeepAliveTime = 60L
  val KeepAliveUnit = TimeUnit.SECONDS
  val Queue         = new SynchronousQueue[Runnable]()
  
  val RejectedExecHandler = new AbortPolicy()
  
  val ThreadFactory = new ThreadFactory {
    val counter = new AtomicInteger(1)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r)
      t.setName( "T" + counter.getAndIncrement() )
      t.setDaemon( false )
      t.setPriority( Thread.NORM_PRIORITY )
      t
    }
  }
  
  val Pool: ExecutorService = new ThreadPoolExecutor(
    CorePoolSize, MaxPoolSize,
    KeepAliveTime, KeepAliveUnit,
    Queue, ThreadFactory, 
    RejectedExecHandler
  )
     
  def shutdown(): Unit = {
    Pool.shutdownNow()
    Pool.awaitTermination(3, TimeUnit.SECONDS)
  }
  
  def submit[T](task: Callable[T]): Future[T] = 
    Pool submit task
  
  def submit[T](code: => T): Future[T] = 
    Pool submit new Callable[T] { def call(): T = code }
}


trait Task[A] {
  
  // executes the task synchronously
  def execute(): A
  
  // submits the task for later execution
  final def submit(): Future[A] = TaskExecutor submit new Callable[A] { def call(): A = execute() }
}



// Task processing an URL
trait UrlTask[A] extends Task[A] {
  
  // url to process
  def url: URL
  
  // processor of the response
  def process(is: InputStream): A
    
  
  // dont keep-alive
  val ConnectionClose = true
  
  // fooling around with user-agent and referer checks
  val UserAgent = "Mozilla/5.0"
  val Referer   = "http://stackoverflow.com/questions/tagged/referer"
  
  // if sending data, define its content-type and the function to fill the OutputStream with the data
  val RequestContentType: Option[String]      = None
  val RequestFn: Option[OutputStream => Unit] = None

  final def execute(): A = {
    val conn: URLConnection = url.openConnection()
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput  true
    conn setDoOutput RequestFn.isDefined
    
    if (ConnectionClose)
       conn setRequestProperty ("Connection", "Close")
    
    if (RequestContentType.isDefined)
      conn setRequestProperty ("Content-Type", RequestContentType.get);
    
    conn setRequestProperty ("User-Agent", UserAgent)
    conn setRequestProperty ("Referer",    Referer)
    
    conn.connect()
    
    RequestFn match {
      case Some(fn) => fn( conn.getOutputStream )
      case None     => // request-parameters solely encoded in URL
    }
    
    val is = conn.getInputStream
    
    try {
      process( is )
    } catch {
      case e:Exception => {
        System.err.println("UrlProcessor failed! URL: " + url)
        e.printStackTrace( System.err )
        throw e
      }
    } finally {
      is.close()
    }
  }
}


// A specialisation that processes XML-data located by the URL
trait XmlTask[A] extends UrlTask[A] {
  import nu.xom.Builder
  
  final def process(is: InputStream): A = {
    val builder = new Builder( )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): A
}


// A specialisation that processes HTML-data located by the URL
// (XML-malformed HTML is forced into XML/XHTML by the tagsoup-parser)
trait HtmlTask[A] extends UrlTask[A] {
  import org.xml.sax.helpers.XMLReaderFactory
  import nu.xom.Builder
  
  val TAGSOUP_PARSER = "org.ccil.cowan.tagsoup.Parser"
  
  final def process(is: InputStream): A = {
    val tagsoup = XMLReaderFactory.createXMLReader( TAGSOUP_PARSER )
    val builder = new Builder( tagsoup )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): A
}