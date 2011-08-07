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


trait Task[R] {
  def task(): Callable[R]
}

trait UrlProcessor[R] extends Task[R] {
  def url: URL
  
  def process(is: InputStream): R
  
  val ConnectionClose     = true
  
  val UserAgent = "Mozilla/5.0"
  val Referer   = "http://stackoverflow.com/questions/tagged/referer"
    
  val RequestContentType: Option[String] = None
  val RequestFn: Option[OutputStream => Unit] = None
  
  
  final def task(): Callable[R] = new Callable[R] {
    def call(): R = {
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
        case None     =>
      }
      
      val is  = conn.getInputStream
      
      val res = try process( is ) catch {
        case e:Exception => {
          System.err.println("UrlProcessor failed! URL: " + url)
          e.printStackTrace( System.err )
          throw e
        }}
      
      is.close()
      res
    }
  }
  
  final def execute(): Future[R] = WorkerPool submit task()
}

trait XmlProcessor[R] extends UrlProcessor[R] {
  import nu.xom.Builder
  
  final def process(is: InputStream): R = {
    val builder = new Builder( )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): R
}

trait HtmlProcessor[R] extends UrlProcessor[R] {
  import org.xml.sax.helpers.XMLReaderFactory
  import nu.xom.Builder
  
  final def process(is: InputStream): R = {
    val tagsoup = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser")
    val builder = new Builder( tagsoup )
    val doc = builder build is
    
    process(doc)
  }
  
  def process(doc: nu.xom.Document): R
}

abstract class Query[R] extends Task[R] {
  def query: String
}

abstract class SearchManager[A] {
  def searchTerm(term: String): List[A]
  def searchByFile(fileInfo: FileInfo): List[A]
}

abstract class Search[A] {
  var out: PrintStream = System.out
  
  def id: String
  
  def search(term: String): List[A]
    
  final def execute(term: String): Future[List[A]] =
     WorkerPool submit { search(term) }
}