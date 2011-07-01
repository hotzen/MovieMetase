package moviemetase
package search

import search._

import java.util.concurrent._
import java.io.PrintStream

import java.net.URL
import java.io.InputStream
import java.util.concurrent.Callable
import java.util.concurrent.Future

trait UrlProcessor[R] {
  def url: URL
  
  def process(is: InputStream): R
    
  final def task(): Callable[R] = new Callable[R] {
    def call(): R = {
      val conn = url.openConnection()
      conn setUseCaches true
      conn setAllowUserInteraction false
      conn setDoInput true
      conn setDoOutput false
      
      conn setRequestProperty ("Referer", "http://stackoverflow.com/questions/tagged/referer")
      conn setRequestProperty ("User-Agent", "Mozilla/5.0") 
      
      conn.connect
      
      try {
        process( conn.getInputStream )
      } catch {
        case e:Exception => {
          System.err.println("UrlProcessor failed! URL: " + url)
          e.printStackTrace( System.err )
          
          throw e
        }
      }
    }
  }
  
  final def execute(): Future[R] = WorkerPool submit task()
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

abstract class Query[R] extends UrlProcessor[List[R]] {
  def query: String
}

abstract class SearchManager[A] {
  type ScoredResult = (Double, A)

  def filter(res: List[ScoredResult]): List[ScoredResult] = res.sortWith( (t1,t2) => t1._1 > t2._1 ).take(3)
  
  def searchTerm(term: String): List[ScoredResult]
  def searchByFile(fileInfo: FileInfo): List[ScoredResult]
}


abstract class Search[A] {
  type ScoredResult = (Double, A)
  
  var out: PrintStream = System.out
  
  def id: String
  
  def search(term: String): List[ScoredResult]
    
  final def execute(term: String): Future[List[ScoredResult]] =
     WorkerPool submit { search(term) }
}