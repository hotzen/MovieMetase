package moviemetase
package sites

import search.WebResult
import java.net.URL

class Yandex {
  // http://www.yandex.com/yandsearch?text=Inception.1080p.BluRay.x264-REFiNED+link%3Aimdb.com&lr=87
  
  case class Query(query: String) extends HtmlTask[List[WebResult]] with DedicatedPool with Throttling with Logging {
    val logID = "Yandex.Query"
    
    StoreCookies = true
    
    RequestHeaders += ("Accept" -> "text/html,application/xhtml+xml,application/xml")
    RequestHeaders += ("Accept-Language" -> "en-us,en")
      
    override val pool = ("YNDX", 1, 2) // 2 concurrent queries

    val throttling = defaultThrottle
    
    val baseURL = "http://www.yandex.com/yandsearch"
    
    // http://help.yandex.com/search/?id=1113759
    val params = "" //sourceid=navclient&ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off&num=10&btnG=Search"
      
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( baseURL )
      sb append "?q=" append q
      sb append "&"   append params
      val url = new URL( sb.toString )
      
      //trace(query, ("url" -> url.toString) :: Nil)
      url
    }
    
    def processDocument(doc: org.jsoup.nodes.Document): List[WebResult] = {
      import org.jsoup.nodes._
      import JSoup._
      doc.select(".b-serp2-list").flatMap(listElem => {
        listElem.select("h2 a").headOption match {
          case Some(a) => {
            val link  = a.attrOpt("href").get
            val title = a.text
            val snippet = listElem.select(".b-serp2-item__text").map(_.text).mkString("")
            
            try   { Some( WebResult(query: String, new URL(link), title, snippet) ) }
            catch { case e:java.net.MalformedURLException => {
              warn("invalid URL: " + e.toString)
              None
            }}
          }
          case None => {
            warn("Google-Result does not contain '#ires li.g a.l'")
            None
          }
        }
      }).toList
    }
  }
}