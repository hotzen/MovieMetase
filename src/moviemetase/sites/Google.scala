package moviemetase
package sites

import Util._
import java.net.URL
import java.io.InputStream
import java.net.HttpURLConnection
import java.util.Random

object Google {
  
  case class Query(query: String) extends Task[List[GoogleResult]] {
      
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
//  import import scala.util.parsing.json.JSON
//  import scala.io.Source
//
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
  val Domains =
    "nosslsearch.google.com" ::
    "google.com.au" ::
    "google.co.uk" ::
    "google.de" ::
    Nil

  def baseURL(domain: String): String = 
    "http://" + domain + "/search" 

  def baseURL: String =
    baseURL( Domains(0) ) // TODO try to change domains before prompting for CAPTCHAs
  
  case class Query(query: String) extends Task[List[GoogleResult]] with DedicatedPool with Logging {
    val logID = "Google.Query"
    
    val pool = ("G", 1, 1) // NO concurrent google-queries

    var ThrottleMin = 100
    var ThrottleMax = 1000
    
    val BlockPagePattern = "/sorry/"
    
    def execute(): List[GoogleResult] = {
      try {
        if (ThrottleMin > 0 && ThrottleMax > 0) {
          val rnd = new Random
          val throttle = ThrottleMin + rnd.nextInt( ThrottleMax - ThrottleMin )
          Thread sleep throttle.toLong
        }

        WebQuery(query).execute()
        
      } catch {
        case e@HttpRedirectException(code, msg, loc, url) => {
          if (loc contains BlockPagePattern) {
            warn("BLOCKED")
            unblockRetry( loc.toURL )
          } else {
            warn("unexpected redirection to " + loc)
            throw e
          }
        }
        case e@HttpResponseException(code, msg, url) if code >= 500 => {
          warn("HTTP " + code + " " + msg + " - aborting")
          Nil
        }
      }
    }
    
    private def unblockRetry(captchaPage: URL): List[GoogleResult] = {
      unblock( captchaPage ) match {
        case Right(_) => {
          info("UNBLOCKED, retrying ...")
          execute() 
        }
        case Left(_)  => {
          warn("STILL BLOCKED, trying to unblock again")
          unblockRetry( captchaPage )
        }
      }
    }
    
    private def unblock(captchaPage: URL): Either[CaptchaFailed, CaptchaSolved] = {
      val challenge = CaptchaReader( captchaPage ).execute()
      val answer   = CaptchaSolver( challenge ).execute()
      CaptchaResponder( answer ).execute()
    }
  }
  
  case class CaptchaChallenge(page: URL, img: URL, formAction: String, formParams: List[(String, String)])  
  case class CaptchaResponse(answer: String, challenge: CaptchaChallenge)
  
  case class CaptchaSolved(answer: CaptchaResponse)
  case class CaptchaFailed(code: Int)

  case class CaptchaRequired(challenge: CaptchaChallenge, cb: CaptchaResponse => Unit) extends HumanTaskEvent[CaptchaResponse] {
    def reply(a: CaptchaResponse) = cb(a)
  }
    
  private case class CaptchaReader(url: URL) extends HtmlTask[CaptchaChallenge] with Logging {
    val logID = "Google.CaptchaReader"

    StoreCookies = true
    
    val CaptchaImagePattern = "/sorry/image"
      
    def processDocument(doc: org.jsoup.nodes.Document): CaptchaChallenge = {
      import org.jsoup.nodes._
      import JSoup._
      
      val imgSrc = doc.select("img").flatMap(img => {
        val src = img.attr("src")
        if (src contains CaptchaImagePattern)
          Some(src)
        else
          None
      }).head // there must be one
      
      val imgPath = {
        if (imgSrc startsWith "/")  imgSrc
        else url.getPath + "/" + imgSrc
      }.replaceAll("//", "/")
      
      val imgUrl = {
        new URL("http://www.google.com" + imgPath)
      }
      trace("CAPTCHA-image: " + imgUrl.toExternalForm)
      
      val formAction = doc.select("form").head.attr("action")
      
      val actionPath = {
        if (formAction startsWith "/") formAction
        else url.getPath + "/" + formAction
      }.replaceAll("//", "/")
      
      val params = doc.select("form").flatMap(f => {
        f.select("input").map(in => {
          val name  = in.attrOpt("name").getOrElse("")
          val value = in.attrOpt("value").getOrElse("")
          (name, value)
        })
      }).toList
      
      CaptchaChallenge(url, imgUrl, actionPath, params)
    }
    
    // captcha-page is displayed with HTTP 50X, ignore it
    override def onError(code: Int, conn: HttpURLConnection): InputStream = conn.getErrorStream
  }
  
  private case class CaptchaSolver(challenge: CaptchaChallenge) extends HumanTask[CaptchaResponse] {
    def createEvent(cb: CaptchaResponse => Unit): HumanTaskEvent[CaptchaResponse] =
      CaptchaRequired(challenge, cb)
  }
  
  private case class CaptchaResponder(resp: CaptchaResponse) extends HttpTask[Either[CaptchaFailed, CaptchaSolved]] with Logging {
    val logID = "Google.CaptchaResponder"
    
    StoreCookies = true
      
    val Charset = "UTF-8"
    
    def url(): URL = {
      val params = { 
        for ((name,value) <- resp.challenge.formParams)
          yield
            name + "=" + {name match {
              case "captcha"  => java.net.URLEncoder.encode(resp.answer, Charset)
              case "continue" => java.net.URLEncoder.encode("http://www.google.com", Charset)
              case _          => java.net.URLEncoder.encode(value, Charset) 
            }}
      }.mkString("&")
      
      val page = resp.challenge.page
      new URL("http://" + page.getHost + resp.challenge.formAction + "?" + params)
    }
    
    var result: Either[CaptchaFailed, CaptchaSolved] = Left( CaptchaFailed(0) )

    // 1
    override def onError(code: Int, conn: HttpURLConnection): InputStream = {
      warn("CAPTCHA-response failed: HTTP " + code)
      result = Left( CaptchaFailed(code) )
      
      conn.getErrorStream // return something to continue
    }
    
    // 2
    override def onRedirect(code: Int, loc: String, conn: HttpURLConnection): HttpURLConnection = {
      //println("CaptchaResponder.onRedirect: " + loc)
      conn // just continue with the same connection
    }
    
    // 3
    override def preProcess(conn: HttpURLConnection, headers: Map[String, List[String]]): Unit = {
      val respCode = conn.getResponseCode 
      if (respCode >= 400)
        result = Left( CaptchaFailed(respCode) )
      else
        result = Right( CaptchaSolved(resp) )
    }

    // 4
    def processResponse(is: InputStream): Either[CaptchaFailed, CaptchaSolved] =
      result
  }
    
  case class WebQuery(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "Google.WebQuery"
    
    StoreCookies = true
      
    RequestHeaders += ("Accept" -> "text/html,application/xhtml+xml,application/xml")
    RequestHeaders += ("Accept-Language" -> "en-us,en")

    // http://code.google.com/intl/de/apis/searchappliance/documentation/46/xml_reference.html#request_parameters
    // http://googlesystem.blogspot.com/2007/04/how-to-disable-google-personalized.html
    // http://www.blueglass.com/blog/google-search-url-parameters-query-string-anatomy/
    //def params: String = "ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off&num=10&adtest=on"
    def params: String = "sourceid=navclient&ie=UTF-8&oe=UTF-8&hl=en&filter=0&safe=0&pws=0&complete=0&instant=off&num=10&btnG=Search"
      
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( baseURL )
      sb append "?q=" append q
      sb append "&"   append params
      val url = new URL( sb.toString )
      
      //trace(query, ("url" -> url.toString) :: Nil)
      url
    }
            
    def processDocument(doc: org.jsoup.nodes.Document): List[GoogleResult] = {
      import org.jsoup.nodes._
      import JSoup._
      doc.select("#ires li.g").flatMap(li => {
        li.select("a.l").headOption match {
          case Some(a) => {
            val link  = a.attrOpt("href").get
            val title = a.text
            val snippet = li.select(".st").map(_.text).mkString("")
            
            try { Some( GoogleResult(query: String, link.toURL, title, snippet) ) }
            catch { case e:java.net.MalformedURLException => None }
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