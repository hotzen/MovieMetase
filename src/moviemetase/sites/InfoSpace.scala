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
  
  def extractLinkFromRedirect(link: String): Option[String] = {
    println( link )
    RedirectRegex.findFirstMatchIn(link) match {
      case Some(m) => Some( java.net.URLDecoder.decode(m.group(1), "UTF-8") )
      case None    => None
    }
  }
}

object MetaCrawler {
  val BASE_URL = "http://www.metacrawler.com/search/web"
  
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "MetaCrawler.Query"
     
    def params: String = ""
    
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      //sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
       
    def process(doc: nu.xom.Document): List[GoogleResult] = {
      import XOM._
      //println( doc.toXML )

      doc.xpath("""//*[@class="searchResult webResult"]""").flatMap(_.toElement).flatMap( resultElem => {

        val optLink = 
          resultElem.xpath("""descendant::*[@class="resultTitle"]""").flatMap(_.toElement).
          flatMap(elem => elem.attribute("href")).
          flatMap( InfoSpace.extractLinkFromRedirect(_) ).headOption
          
        val title =
          resultElem.xpath("""descendant::*[@class="resultTitle"]""").flatMap(_.toElement).
          map( elem => elem.value ).headOption.getOrElse("NO-TITLE")
        
        val snippet =
          resultElem.xpath("""descendant::*[@class="resultDescription"]""").flatMap(_.toElement).
          map(_.value).headOption.getOrElse("NO-SNIPPET")
          
        optLink match {
          case Some(link) => Some( GoogleResult(query: String, link.toURL, title, snippet) )
          case None       => None
        }
      })
    }
  }
}


object DogPile {
  val BASE_URL = "http://www.dogpile.com/info.dogpl/search/web"
  
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "DogPile.Query"
     
    def params: String = ""
    
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      //sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
       
    def process(doc: nu.xom.Document): List[GoogleResult] = {
      import XOM._
      //println( doc.toXML )

      doc.xpath("""//*[@class="searchResult webResult"]""").flatMap(_.toElement).flatMap( resultElem => {

        val optLink = 
          resultElem.xpath("""descendant::*[@class="resultTitle"]""").flatMap(_.toElement).
          flatMap(elem => elem.attribute("href")).
          flatMap( InfoSpace.extractLinkFromRedirect(_) ).headOption
          
        val title =
          resultElem.xpath("""descendant::*[@class="resultTitle"]""").flatMap(_.toElement).
          map( elem => elem.value ).headOption.getOrElse("NO-TITLE")
        
        val snippet =
          resultElem.xpath("""descendant::*[@class="resultDescription"]""").flatMap(_.toElement).
          map(_.value).headOption.getOrElse("NO-SNIPPET")
          
        optLink match {
          case Some(link) => Some( GoogleResult(query: String, link.toURL, title, snippet) )
          case None       => None
        }
      })
    }
  }
}