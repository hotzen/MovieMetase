package moviemetase
package sites

import Util._
import nu.xom._
import java.net.URL
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.json.JSON
import java.util.concurrent.atomic.AtomicInteger

object Google {
  val UseAjax        = 1
  val UseWeb         = 2
  val UseMetaCrawler = 3
  val UseDogPile     = 4
  val UseNothing     = 5
  
  val use = new AtomicInteger( UseAjax )
  
  // generic query that tries different query-methods
  case class Query(query: String) extends Task[List[GoogleResult]] with Logging {
    val logID = "GoogleAjax.Query"
      
    def execute(): List[GoogleResult] = use.get match {
      
      case id@UseAjax          => tryOrNext(id, GoogleAjax.Query(query)   )
      case id@UseWeb           => tryOrNext(id, GoogleWeb.Query(query)    )
      case id@UseMetaCrawler   => tryOrNext(id, MetaCrawler.Query(query)  )
      case id@UseDogPile       => tryOrNext(id, DogPile.Query(query)      )
      
      case id if id >= UseNothing => {
        warn("No more Query-Methods to try, aborting.")
        Nil
      }
      case id => {
        warn("Unknown Query-Method #" + id + ", resetting to AJAX")
        use.compareAndSet(id, UseAjax)
        Nil
      }
    }
    
    def tryOrNext(id: Int, task: Task[List[GoogleResult]]): List[GoogleResult] = {
      try {
        val label = task match {
          case l:Logging => l.logID
          case _         => task.toString
        }
        trace("trying " + label + " ...")
        
        task.execute()

      } catch { case e:Exception => {
        warn(task + " failed with " + e.getMessage() + ", switching to next query")
        use.compareAndSet(id, id+1)
        execute()
      }}
    }
  }
}

case class GoogleResult(query: String, url: URL, title: String, snippet: String) //, pageMap: List[GooglePageMapDataObject]) {


object GoogleAjax {
  val BASE_URL = "http://ajax.googleapis.com/ajax/services/search/web"
  
  case class Query(query: String, page: Int = 1) extends UrlTask[List[GoogleResult]] with Logging {
    val logID = "GoogleAjax.Query"
    
    def params: String = "v=1.0&rsz=large&hl=en"
    def limit: Int = 8
      
    def url: URL = {
      val sb = new StringBuilder( BASE_URL )
      sb append "?"       append params
      sb append "&start=" append ((page-1)*limit)
      sb append "&q="     append query.urlEncode

      val url = sb.toString 
      trace("querying '" + query + "' ...", ("url" -> url) :: Nil)
      new URL(url)
    }
    
    def process(in: InputStream): List[GoogleResult] = {
      import Util._
      import JsonType._

      val str = Source.fromInputStream(in).mkString
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
    
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")

      val sb = new StringBuilder( BASE_URL )
      sb append "?q=" append q
      sb append "&"   append params

      trace(query, ("url" -> sb.toString) :: Nil)
      new URL( sb.toString )
    }
       
    def process(doc: nu.xom.Document): List[GoogleResult] = {
      import nu.xom._
      import XOM._
      
      val ctx = Context.XHTML

      doc.xpath("""//xhtml:li[@class="g"]""", ctx).flatMap(_.toElement).flatMap( resultElem => {
        val linkTitle = 
          resultElem.xpath("""descendant::xhtml:a[@class="l"]""", ctx).flatMap(_.toElement).
          flatMap(a => a.attribute("href") match {
            case Some(href) => Some( (href, a.value) )
            case None       => None
          }).head // shall fail
                
        val snippet =
          resultElem.xpath("""descendant::*[@class="st"]""").flatMap(_.toElement).
          map(_.value).headOption.get // shall fail
        
        Some( GoogleResult(query: String, linkTitle._1.toURL, linkTitle._2, snippet) )
      })
    }
  }
}

object GoogleCSE {
  val API_KEY  = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
  val BASE_URL = "https://www.googleapis.com/customsearch/v1"
    
  val NS_ATOM = "http://www.w3.org/2005/Atom"
  val NS_CSE  = "http://schemas.google.com/cseapi/2010"
  val NS_OS   = "http://a9.com/-/spec/opensearch/1.1/"
  val NS_GD   = "http://schemas.google.com/g/2005"

  case class Query(cseID: String, query: String, page: Int = 1) extends UrlTask[List[GoogleResult]] with Logging {
    val logID = "GoogleCSE.Query"
    
    def params: String = "alt=atom&prettyprint=true&safe=off"
    def limit: Int = 10
    
    def url: URL = {
      val sb = new StringBuilder( BASE_URL )
      sb append "?key="   append API_KEY
      sb append "&cx="    append cseID
      sb append "&"       append params
      sb append "&num="   append limit
      sb append "&start=" append (page + (page-1)*limit)
      sb append "&q="     append query.urlEncode
      
      val url = sb.toString 
      trace("querying '" + query + "' ...", ("url" -> url) :: Nil)
      new URL(url)
    }

    def process(in: InputStream): List[GoogleResult] = {
      import XOM._
      
      val builder = new Builder()
      val doc = builder build in
      //println( doc.toXML )
      
      val results = for (entry <- doc.getRootElement.getChildElements("entry", NS_ATOM)) yield {
  
//        val id = entry.
//          getChildElements("id", NS_ATOM).
//          map( _.getValue ).
//          head
        
        val url = entry.
          getChildElements("link", NS_ATOM).
          map( _.getAttributeValue("href") ).
          head
  
        val title = entry.
          getChildElements("title", NS_ATOM).
          map( _.getValue.noTags.noEntities ).
          head
  
        val snippet = entry.
          getChildElements("summary", NS_ATOM).
          map( _.getValue.noTags.noEntities ).
          head
        
        GoogleResult(query, new URL(url), title, snippet)
      }
      
      results.toList
    }
  }
}