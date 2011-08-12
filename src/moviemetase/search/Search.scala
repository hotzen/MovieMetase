package moviemetase
package search

import search._
import java.util.concurrent._
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.net.URLConnection


// Something that can wrap itself into a task for concurrent execution
trait Task[A] {
  def task(): Callable[A]
}


// Some task that processes data located by an URL
trait UrlProcessor[A] extends Task[A] {
  
  // url to process
  def url: URL
  
  // processor of the InputStream representing the URL's data
  def process(is: InputStream): A
  
  // dont keep-alive
  val ConnectionClose = true
  
  // fooling around with stupid user-agent and referer checks
  val UserAgent = "Mozilla/5.0"
  val Referer   = "http://stackoverflow.com/questions/tagged/referer"
    
  // if sending data, define its content-type and
  // the function to fill the OutputStream with the data
  val RequestContentType: Option[String] = None
  val RequestFn: Option[OutputStream => Unit] = None
  
  // create a Callable-Task to be executed concurrently
  final def task(): Callable[A] = new Callable[A] {
    def call(): A = {
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
      
      val is  = conn.getInputStream
      val res = process( is )
//      try process( is ) catch {
//        case e:Exception => {
//          System.err.println("UrlProcessor failed! URL: " + url)
//          e.printStackTrace( System.err )
//          throw e
//        }}
      is.close()
      res
    }
  }
  
  
  // automatically execute the task in the global Worker-Pool and
  // return the task's result wrapped in a Future
  final def execute(): Future[A] = WorkerPool submit task()
}


// A specialisation that processes XML-data located by the URL
trait XmlProcessor[A] extends UrlProcessor[A] {
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
trait HtmlProcessor[A] extends UrlProcessor[A] {
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


// A Query-Task that queries something and returns A  
abstract class Query[A] extends Task[A] {
  def query: String
}


abstract class SearchManager[A] {
  def searchTerm(term: String): List[A]
  def searchByFile(fileInfo: FileInfo): List[A]
}


// A Search that possibly uses and combines Queries to return an overall A
abstract class Search[A] {
  var out: PrintStream = System.out
  
  def id: String
  
  def search(term: String): List[A]
    
  final def execute(term: String): Future[List[A]] =
     WorkerPool submit { search(term) }
}