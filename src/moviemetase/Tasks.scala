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
  
//  val TaskInfos = new WeakHashMap[Callable[_], TaskInfo]()
  
  def submit[T](task: Callable[T]): Future[T] =
      Pool submit task
//  {
//    val fut = Pool submit task
//    TaskInfos.put(task, TaskInfo(new WeakReference(fut)))
//    fut
//  }
    
  
  def submit[T](code: => T): Future[T] = 
    submit( new Callable[T] { def call(): T = code } )
}

trait Task[A] {
  // executes the task synchronously
  def execute(): A
  
  // submits the task for later execution
  final def submit(): Future[A] = TaskExecutor submit new Callable[A] { def call(): A = execute() }
  
  // create a new task that executes <this> and then <next>
  final def then[B](next: Task[B]): Task[B] = new SerialTask[B](this, next)
  
  // create a new task that executes <this> and then the tasks produced by <f>
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
    
    // execute first task 
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
  val connectionClose = true
  
  val userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:9.0.1) Gecko/20100101 Firefox/9.0.1"
  val referer   = "http://stackoverflow.com/questions/tagged/referer" 
  
  // if sending data, define its content-type and the function to fill the OutputStream with the data
  val requestMethod: String = "GET"
  val requestProperties: List[(String, String)] = Nil
  val requestContentType: Option[String]      = None
  val requestSendData: Option[OutputStream => Unit] = None

  final def execute(): A = {
    val conn: HttpURLConnection = url.openConnection() match {
      case http:HttpURLConnection => http
      case _                      => throw new Exception("UrlTask only supports HTTP connections")
    }
    
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput  true
    conn setDoOutput requestSendData.isDefined
    
    conn setRequestMethod requestMethod
    conn setInstanceFollowRedirects false
        
    if (connectionClose)
       conn setRequestProperty ("Connection", "Close")
    
    if (requestContentType.isDefined)
      conn setRequestProperty ("Content-Type", requestContentType.get);
    
    conn setRequestProperty ("User-Agent", userAgent)
    conn setRequestProperty ("Referer",    referer)
    
    for ((propName, propValue) <- requestProperties)
      conn setRequestProperty (propName, propValue)
    
    conn.connect()
    
    requestSendData match {
      case Some(f) => f( conn.getOutputStream )
      case None    => // request-parameters only encoded in URL
    }
    
    val respCode = conn.getResponseCode
    if (respCode != HttpURLConnection.HTTP_OK)
      throw new Exception("UrlTask failed with HTTP " + respCode + ": " + conn.getResponseMessage)
        
    val is = conn.getInputStream
    
    try {
      val res = process( is )
      
      if (!connectionClose)
        conn.disconnect()
        
      res
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