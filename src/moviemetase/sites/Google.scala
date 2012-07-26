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
import java.util.Random
import scala.concurrent.SyncVar
import moviemetase.ui.comp.JImage

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
  
  case class Query(query: String) extends Task[List[GoogleResult]] with DedicatedPool with Logging {
    val logID = "Google.Query"
    
    val pool = ("G", 1, 1) // NO concurrent google-queries

    val ThrottleMin = 50 // 1000
    val ThrottleMax = 100 // 3000
    
    def execute(): List[GoogleResult] = {
      try {
        val rnd = new Random
        val throttle = ThrottleMin + rnd.nextInt( ThrottleMax - ThrottleMin )
        println("throttling " + (throttle.toFloat/1000) + " secs")
        Thread sleep throttle.toLong        
        
        WebQuery(query).execute()
        
      } catch {
        // we got blocked
        case e@HttpRedirectException(code, msg, loc, url) if loc contains "/sorry/" => {
          tryUnblock( loc.toURL )
          Nil
        }
        
        // access denied due to block?
        case e@HttpResponseException(code, msg, url) if code >= 500 => {
          println("HTTP50x: " + e)
          Nil
        }
        
        case e:Exception => {
          println("UNEXPECTED ERROR")
          e.printStackTrace
          
          Nil
        }
      }
    }
    
    private def tryUnblock(captchaPage: URL): List[GoogleResult] = {
      unblock( captchaPage ) match {
        case Right(_) => {
          info("UNBLOCKED, continue ...")
          execute() 
        }
        case Left(_)  => {
          warn("STILL BLOCKED, trying to unblock again")
          tryUnblock( captchaPage )
        }
      }
    }
    
    private def unblock(captchaPage: URL): Either[CaptchaFailed, CaptchaConfirmed] = {
      val question = CaptchaReader( captchaPage ).execute()
      println("question: " + question)
      println("CAPTCHA: " + question.img)
      
      val answer = CaptchaSolver( question ).execute()
      println("answer: " + answer)
      
      val result = CaptchaResponder( answer ).execute()
      println("result: " + result)
      
      result
    }
  }
  
  private case class CaptchaQuestion(page: URL, img: URL, formAction: String, formParams: List[(String, String)])  
  private case class CaptchaAnswer(solution: String, question: CaptchaQuestion)
  private case class CaptchaConfirmed(answer: CaptchaAnswer)
  private case class CaptchaFailed(code: Int)
  
  private case class CaptchaReader(url: URL) extends HtmlTask[CaptchaQuestion] with Logging {
    val logID = "Google.CaptchaReader"

    def processDocument(doc: org.jsoup.nodes.Document): CaptchaQuestion = {
      import org.jsoup.nodes._
      import JSoup._
      
      val imgPath = doc.select("img").flatMap(_.attrOpt("src") match {
        case Some(src) if src contains "/sorry/image" => Some(src)
        case _ => None
      }).head
      
      val img = new URL("http://www.google.com" + imgPath)
      
      val formAction = doc.select("form").map(f => f.attr("action")).head
      
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
      
      val q = CaptchaQuestion(url, img, actionPath, params)
      trace("parsed captcha-page: " + q)
      q
    }
    
    // captcha-page is displayed with HTTP 50X, ignore it
    override def onError(code: Int, conn: HttpURLConnection): InputStream = {
      println("ignoring HTTP " + code)
      conn.getErrorStream
    }
      
  }
  
  class UnsolvedCaptchaException() extends Exception
  
  private case class CaptchaSolver(question: CaptchaQuestion) extends Task[CaptchaAnswer] with Logging {
    val logID = "Google.CaptchaSolver"
      
    def execute(): CaptchaAnswer = {
      val result = prompt( question.img )
      
      val a = result.get match {
        case Right(a) => CaptchaAnswer(a, question)
        case Left(t)  => throw t
      }
      
      trace("solved captcha: " + a)
      a
    }
    
    private def prompt(imgUrl: URL): SyncVar[Either[Throwable,String]] = {
      val res = new SyncVar[Either[Throwable,String]]
      ui.UI.run {
        import java.awt._
        import javax.swing._
        
        val panel = new JPanel(new BorderLayout)
                
        val lbl = new JLabel()
        lbl.setText("Please enter the CAPTCHA:")
        panel.add(lbl, BorderLayout.NORTH)
        
        val img = new JImage(imgUrl, None, JImage.Blocking, JImage.NoCaching)
        panel.add(img, BorderLayout.SOUTH)
        
        val prompt = JOptionPane.showInputDialog(panel)
        
        if (prompt == null)
          res put Left(new UnsolvedCaptchaException)
        else
          res put Right(prompt)
      }
      res
    }
  }
  
  private case class CaptchaResponder(answer: CaptchaAnswer) extends Task[Either[CaptchaFailed, CaptchaConfirmed]] with Logging {
    val logID = "Google.CaptchaResponder"
    
    val Charset = "UTF-8"
      
    def execute(): Either[CaptchaFailed, CaptchaConfirmed] = {
      val params = { 
        for ((name,value) <- answer.question.formParams)
          yield
            name + "=" + {name match {
              case "captcha"  => java.net.URLEncoder.encode(answer.solution, Charset)
              case "continue" => java.net.URLEncoder.encode("http://www.google.com", Charset)
              case _          => java.net.URLEncoder.encode(value, Charset) 
            }}
      }.mkString("&")
      
      val page = answer.question.page
      val url = new URL("http://" + page.getHost + answer.question.formAction + "?" + params)
      
      trace("responding", ("url" -> url) :: Nil)
      
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setUseCaches(false)
      conn.setDoInput(true)
      conn.setDoOutput(false)
      conn.setRequestProperty("Referer", "http://www.google.com/sorry/Captcha")
      conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:5.0) Gecko/20100101 Firefox/5.0")
      conn.setRequestProperty("Accept", "text/html,application/xhtml+xml,application/xml") 
      conn.setRequestProperty("Accept-Language", "en-us,en")
      conn.setRequestProperty("Accept-Charset", "utf-8")
      conn.connect()
            
      val respCode = conn.getResponseCode
      
      val respOK    = (respCode == 200)
      val respRedir = (respCode >= 300 && respCode < 400)
      
      val headers = scala.collection.convert.Wrappers.JMapWrapper( conn.getHeaderFields ).toList
      val newLoc = headers.exists(_ match {
        case ("Location", _) => true
        case _ => false
      })
      
      if ( (respOK || respRedir) && newLoc)
        Right( CaptchaConfirmed(answer) )
      else
        Left( CaptchaFailed(conn.getResponseCode) )
    }
  }
    
  case class WebQuery(query: String) extends HtmlTask[List[GoogleResult]] with Logging {
    val logID = "Google.WebQuery"
    
    RequestHeaders += ("Accept" -> "text/html,application/xhtml+xml,application/xml")
    RequestHeaders += ("Accept-Language" -> "en-us,en")

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