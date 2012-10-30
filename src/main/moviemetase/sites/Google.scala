package moviemetase
package sites

import java.net.URL
import java.io.InputStream
import java.net.HttpURLConnection
import java.util.Random
import moviemetase.search.WebResult

case class GoogleResult(query: String, url: URL, title: String, snippet: String) {

}

object Google {
  import language.implicitConversions
  implicit def google2web(g: GoogleResult): WebResult =
    WebResult(g.query, g.url, g.title, g.snippet)

  def baseURL: String = "http://nosslsearch.google.com/search"
  
  case class Query(query: String) extends Task[List[GoogleResult]] with DedicatedPool with Throttling with Logging {
    val logID = "Google.Query"
    
    val pool = ("GOGL", 1, 1) // NO concurrent google-queries
    val throttling = defaultThrottle
    
    val BlockPagePattern = "/sorry/"
    
    def execute(): List[GoogleResult] = {
      try RawQuery(query).execute()
      catch {
        case e@HttpRedirectException(code, msg, loc, url) => {
          if (loc contains BlockPagePattern) {
            warn("BLOCKED")
            unblockRetry( new URL(loc) )
          } else {
            warn("unexpected redirection to " + loc)
            throw e
          }
        }
        case e@HttpResponseException(code, msg, url) /* if code >= 500 */ => {
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
      val response  = CaptchaSolver( challenge ).execute()
      CaptchaResponder( response ).execute()
    }
  }
  
  case class CaptchaChallenge(page: URL, img: URL, formAction: String, formParams: List[(String, String)])  
  case class CaptchaResponse(answer: String, challenge: CaptchaChallenge)
  
  case class CaptchaSolved(answer: CaptchaResponse)
  case class CaptchaFailed(code: Int)

  case class CaptchaRequired(challenge: CaptchaChallenge, cb: CaptchaResponse => Unit) extends HumanTaskEvent[CaptchaResponse] {
    def feedback(a: CaptchaResponse) = cb(a)
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
    
  case class RawQuery(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "Google.RawQuery"
    
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
            
            try   { Some( GoogleResult(query: String, new URL(link), title, snippet) ) }
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