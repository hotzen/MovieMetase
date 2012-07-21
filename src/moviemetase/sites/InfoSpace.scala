package moviemetase
package sites

import Util._
import nu.xom._
import java.net.URL
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.json.JSON

object InfoSpace {
  val RedirectRegex = """ru=([^&]+)""".r
  
  def extractLinkFromRedirect(link: String): Option[String] =
    RedirectRegex.findFirstMatchIn(link) match {
      case Some(m) => Some( java.net.URLDecoder.decode(m.group(1), "UTF-8") )
      case None    => None
    }
}

object MetaCrawler {
  val BASE_URL = "http://www.metacrawler.com/search/web"
  
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "MetaCrawler.Query"
     
    val params: String = ""
    
    lazy val url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      //sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
    
    def processDocument(doc: org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      doc.select("#resultsPane").headOption match {
        case Some(res) => res.select(".webResult").map(li => {
          val a     = li.select("a.resultTitle").head // fails
          val link  = InfoSpace.extractLinkFromRedirect( a.attr("href") ).get // fails
          val title = a.text

          val snippet = li.select(".resultDescription").map(_.text).head // fails

          GoogleResult(query: String, link.toURL, title, snippet)
        }).toList
        case None => throw new Exception("no <* class=webResult> results-container found")
      }
    }
  }
}


object DogPile {
  val BASE_URL = "http://www.dogpile.com/info.dogpl/search/web"
  
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "DogPile.Query"
     
    val params: String = ""
    
    lazy val url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      //sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
    
    def processDocument(doc:  org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      doc.select("#resultsPane").headOption match {
        case Some(res) => res.select(".webResult").map(li => {
          val a     = li.select("a.resultTitle").head // fails
          val link  = InfoSpace.extractLinkFromRedirect( a.attr("href") ).get // fails
          val title = a.text

          val snippet = li.select(".resultDescription").map(_.text).head // fails
          
          GoogleResult(query: String, link.toURL, title, snippet)
        }).toList
        case None => throw new Exception("no <* class=webResult> results-container found")
      }
    }
  }
}