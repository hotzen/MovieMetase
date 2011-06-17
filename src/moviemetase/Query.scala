package moviemetase

import java.net.URL
import java.io.InputStream
import java.util.concurrent.Callable

trait Query[R] {
  def query: String
  def url: URL
  def parse(is: InputStream): List[R]
}

case class QueryExecutor[R](q: Query[R]) extends Callable[List[R]] {
  def call(): List[R] = {
    
    println("executing query-url " + q.url.toString)
    
    val conn = q.url.openConnection()
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput true
    conn setDoOutput false
  
    //conn setRequestProperty ("Accept", "text/xml;q=0.9,application/xml;q=0.8,application/xhtml+xml;q=0.7,text/html;q=0.3,text/*;q=0.1")
    //conn setRequestProperty ("Accept-Charset", "utf-8;q=0.9,ISO-8859-1;q=0.5,*;q=0.1")
    //conn setRequestProperty ("Accept-Encoding", "identity")
    //conn setRequestProperty ("Accept-Language", "en-US;q=0.9,de-DE;q=0.5")
    
    conn setRequestProperty ("Referer", "http://stackoverflow.com/questions/tagged/referer")
    conn setRequestProperty ("User-Agent", "Mozilla/5.0") // (Windows NT 6.1; WOW64) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.91 Safari/534.30") 
    
    conn.connect
    
    q.parse( conn.getInputStream )
  }
}