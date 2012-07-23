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
import java.net.HttpURLConnection

object Google {
//  val UseAjax        = 1
//  val UseWeb         = 2
//  val UseMetaCrawler = 3
//  val UseDogPile     = 4
//  val UseNothing     = 5
  
//  val use = new AtomicInteger( UseMetaCrawler ) //XXX Skip AJAX since it finds considerably fewer results than the web-search :/
  
  // generic query that tries different query-methods
  case class Query(query: String) extends Task[List[GoogleResult]] {
    
    //def execute(): List[GoogleResult] = MetaCrawler.Query(query).execute()
    def execute(): List[GoogleResult] = GoogleWeb.Query(query).execute()
    
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
//object GoogleAjax {
//  val BASE_URL = "http://ajax.googleapis.com/ajax/services/search/web"
//  
//  case class Query(query: String, page: Int = 1) extends HttpTask[List[GoogleResult]] with Logging {
//    val logID = "GoogleAjax.Query"
//    
//    def params: String = "v=1.0&rsz=large&hl=en"
//    def limit: Int = 8
//      
//    lazy val url: URL = {
//      val sb = new StringBuilder( BASE_URL )
//      sb append "?"       append params
//      sb append "&start=" append ((page-1)*limit)
//      sb append "&q="     append query.urlEncode
//
//      val url = sb.toString 
//      trace("querying '" + query + "' ...", ("url" -> url) :: Nil)
//      new URL(url)
//    }
//    
//    def processResponse(in: InputStream): List[GoogleResult] = {
//      import Util._
//      import JsonType._
//
//      val str = Source.fromInputStream(in).mkString
//      
//      println(str.replaceAll("}", "}\n"))
//      
//      for {
//        M(map)      <- List( JSON.parseFull(str).get ) // shall fail
//        M(respData) =  map("responseData")
//        L(results)  =  respData("results")
//        M(resMap)   <- results
//        S(url)      =  resMap("unescapedUrl")
//        S(title)    =  resMap("titleNoFormatting")
//        S(snippet)  =  resMap("content")
//      } yield {
//        GoogleResult(query, url.toURL, title, snippet.noTags.noEntities)
//      }
//    }
//  }
//}

object GoogleWeb {
  //val BASE_URL = "http://nosslsearch.google.com/search"
  
  val Domains =
    "nosslsearch.google.com" ::
    "google.com.au" ::
    "google.co.uk" ::
    "google.de" ::
    Nil

  def baseURL(domain: String): String = 
    "http://" + domain + "/search" 

  def baseURL: String =
    baseURL( Domains(0) )

  sealed trait QueryState
  object QueryState {
    case object Normal extends QueryState
    case class Blocked(captcha: URL) extends QueryState
    case object Unblocking extends QueryState
    //case object 
  }
    
  case class Query(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    import QueryState._
    
    val logID = "GoogleWeb.Query"

    var state: QueryState = Normal
      
    override def target = "G"
    override val MinPoolSize = 1
    override val MaxPoolSize = 1
    
    Throttle = Some( (1000, 3000) ) // pause between 1-3 secs between requests
    StoreCookies = true
    
    RequestProperties += ("Accept" -> "text/html,application/xhtml+xml,application/xml")
    RequestProperties += ("Accept-Language" -> "en-us,en")

    // http://code.google.com/intl/de/apis/searchappliance/documentation/46/xml_reference.html#request_parameters
    // http://googlesystem.blogspot.com/2007/04/how-to-disable-google-personalized.html
    // http://www.blueglass.com/blog/google-search-url-parameters-query-string-anatomy/
    //def params: String = "ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off&num=10&adtest=on"
    def params: String = "sourceid=navclient&ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off&num=10&btnG=Search"
            
    var DomainIndex = 0
      
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val base = baseURL( Domains(DomainIndex) )
      
      val sb = new StringBuilder( base )
      sb append "?q=" append q
      sb append "&"   append params
      val url = new URL( sb.toString )
      
      trace(query, ("url" -> url.toString) :: Nil)
      url
    }

    override def onRedirect(code: Int, loc: String, conn: HttpURLConnection): HttpURLConnection = state match {
      case Normal if loc contains "/sorry/" => {
        warn("BLOCKED BY GOOGLE")
        
        val url = loc.toURL  
        state = Blocked(url)
        prepare(url)
      }
      case Unblocking => {
        prepare(loc.toURL)
      }
      case _ =>
        super.onRedirect(code, loc, conn)
    }
    
    override def onError(code: Int, conn: HttpURLConnection): InputStream = state match {
      case Blocked(captcha) if conn.getURL.toString contains captcha.toString =>
        conn.getErrorStream
      case _ =>
        super.onError(code, conn)
    }
            
    def processDocument(doc: org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      state match {
        case Normal =>
          processResultsDocument( doc )
        
        case Blocked(captcha) =>
          processCaptchaDocument( doc )

        case _ =>
          throw new IllegalStateException("processDocument in state " + state)
      }
    }
    
    def processResultsDocument(doc: org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      doc.select("#ires li.g").flatMap(li => {
        li.select("a.l").headOption match {
          case Some(a) => {
            val link  = a.attrOpt("href").get
            val title = a.text
            val snippet = li.select(".st").map(_.text).mkString("")
            
            Some( GoogleResult(query: String, link.toURL, title, snippet) )
          }
          case None => {
            warn("Google-Result does not contain '#ires li.g a.l'")
            None
          }
        }
      }).toList
    }
    
    def processCaptchaDocument(doc: org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      
      val img = doc.select("img").flatMap(_.attrOpt("src") match {
        case Some(src) if src contains "/sorry/image" => Some(src)
        case _ => None
      }).headOption
                
      val params = doc.select("form").flatMap(f => {
        f.select("input").map(in => {
          val name  = in.attrOpt("name").getOrElse("")
          val value = in.attrOpt("value").getOrElse("")
          (name, value)
        })
      }).toList
      
      println("captcha-img: " + img)
      println("params: " + params.mkString("\n"))
      
      state = Unblocking
            
      Nil
    }
  }
}