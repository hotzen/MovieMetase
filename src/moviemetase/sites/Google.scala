package moviemetase
package sites

import Util._
import nu.xom._
import java.net.URL
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.json.JSON
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Future, future}

object Google {
  val UseAjax        = 1
  val UseWeb         = 2
  val UseMetaCrawler = 3
  val UseDogPile     = 4
  val UseNothing     = 5
  
  val use = new AtomicInteger( UseMetaCrawler ) //XXX Skip AJAX since it finds considerably fewer results than the web-search :/
  
  // generic query that tries different query-methods
  case class Query(query: String) extends Task[List[GoogleResult]] {
    
    def execute(): List[GoogleResult] = MetaCrawler.Query(query).execute()
    
//    def executeQuery(): List[GoogleResult] = use.get match {
//      case id@UseAjax          => tryOrNext(id, GoogleAjax.Query(query)   )
//      case id@UseWeb           => tryOrNext(id, GoogleWeb.Query(query)    )
//      case id@UseMetaCrawler   => tryOrNext(id, MetaCrawler.Query(query)  )
//      case id@UseDogPile       => tryOrNext(id, DogPile.Query(query)      )
//      
//      case id if id >= UseNothing => {
//        warn("No more Query-Methods to try, aborting.")
//        Nil
//      }
//      case id => {
//        warn("Unknown Query-Method #" + id + ", resetting to AJAX")
//        use.compareAndSet(id, UseAjax)
//        Nil
//      }
//    }
//    
//    def tryOrNext(id: Int, task: Task[List[GoogleResult]]): List[GoogleResult] = {
//      try {
//        val label = task match {
//          case l:Logging => l.logID
//          case _         => task.toString
//        }
//        //trace("trying " + label + " ...")
//        task.execute()
//
//      } catch { case e:Exception => {
//        warn(task + " failed with " + e.getMessage() + ", switching to next method")
//        use.compareAndSet(id, id+1)
//        execute()
//      }}
//    }
  }
}

case class GoogleResult(query: String, url: URL, title: String, snippet: String) {
//  override def toString = {
//    val sb = new StringBuilder
//    sb append "GoogleResult(" append query append "){\n"
//    sb append "  url:     " append url.toString append "\n"
//    sb append "  title:   " append title        append "\n"
//    sb append "  snippet: " append snippet      append "\n"
//    sb append "}"
//    sb.toString
//  }
}

////XXX very poor result-quality :( 
object GoogleAjax {
  val BASE_URL = "http://ajax.googleapis.com/ajax/services/search/web"
  
  case class Query(query: String, page: Int = 1) extends HttpTask[List[GoogleResult]] with Logging {
    val logID = "GoogleAjax.Query"
    
    def params: String = "v=1.0&rsz=large&hl=en"
    def limit: Int = 8
      
    lazy val url: URL = {
      val sb = new StringBuilder( BASE_URL )
      sb append "?"       append params
      sb append "&start=" append ((page-1)*limit)
      sb append "&q="     append query.urlEncode

      val url = sb.toString 
      trace("querying '" + query + "' ...", ("url" -> url) :: Nil)
      new URL(url)
    }
    
    def processResponse(in: InputStream): List[GoogleResult] = {
      import Util._
      import JsonType._

      val str = Source.fromInputStream(in).mkString
      
      println(str.replaceAll("}", "}\n"))
      
      for {
        M(map)      <- List( JSON.parseFull(str).get ) // shall fail
        M(respData) =  map("responseData")
        L(results)  =  respData("results")
        M(resMap)   <- results
        S(url)      =  resMap("unescapedUrl")
        S(title)    =  resMap("titleNoFormatting")
        S(snippet)  =  resMap("content")
      } yield {
        GoogleResult(query, url.toURL, title, snippet.noTags.noEntities)
      }
    }
  }
}

object GoogleWeb {
  val BASE_URL = "http://nosslsearch.google.com/search"
  
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "GoogleWeb.Query"
     
    // http://code.google.com/intl/de/apis/searchappliance/documentation/46/xml_reference.html#request_parameters
    // http://googlesystem.blogspot.com/2007/04/how-to-disable-google-personalized.html
    // http://www.blueglass.com/blog/google-search-url-parameters-query-string-anatomy/
    def params: String = "ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off"
    
    lazy val url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
    
     def processDocument(doc:  org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      doc.select("#ires li.g").map(li => {
        val a     = li.select("a.l").head // fails
        val link  = a.attrOpt("href").get // fails
        val title = a.text

        val snippet = li.select(".st").map(_.text).head // fails
        
        GoogleResult(query: String, link.toURL, title, snippet)
      }).toList
    }
  }
}