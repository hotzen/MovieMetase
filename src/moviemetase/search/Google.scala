package moviemetase
package search

import java.net.URL
import nu.xom._
//import org.xml.sax.helpers.XMLReaderFactory
import java.io.InputStream
//import java.io.BufferedReader
import scala.io.Source
import scala.util.parsing.json.JSON

import Util._

sealed trait GoogleQuery extends Query[List[GoogleResult]] with UrlProcessor[List[GoogleResult]]

object Google {
  
  val NonAlphaNumRegex = """[^a-zA-Z\d]+""".r
  //val MultiMinusRegex  = """-{2,}""".r
  val FirstMinusRegex  = """^-+""".r
  val LastMinusRegex   = """-+$""".r
  
  def fuzzyTerm(t: String): String = {
    val _1 = t.trim
    val _2 = NonAlphaNumRegex.replaceAllIn(_1, "-")
    val _3 = FirstMinusRegex.replaceFirstIn(_2, "")
    val _4 = LastMinusRegex.replaceFirstIn(_3, "")
    _4
  }
  
  def strictTerm(t: String): String = "\"" + t.trim + "\""
  def bothTerms(t1: String, t2: String): String = t1.trim + " | " + t2.trim
}

object GoogleAjax {
  val BASE_URL = "http://ajax.googleapis.com/ajax/services/search/web"
  
  case class Query(query: String, page: Int = 1) extends GoogleQuery {
    def params: String = "v=1.0&rsz=large&hl=en"
    def limit: Int = 8
      
    def url: URL = {
      val urlBuilder = new StringBuilder( BASE_URL )
      urlBuilder append "?"       append params
      urlBuilder append "&start=" append ((page-1)*limit)
      urlBuilder append "&q="     append query.urlEncode
            
      new URL( urlBuilder.toString )
    }
    
    def process(in: InputStream): List[GoogleResult] = {
      import Util._
      
      val str = Source.fromInputStream(in).mkString
      val json = JSON.parse( str ).head
      
      for ( (k1:String,resp:List[Any]) <- json if k1 == "responseData";
            (k2:String,ress:List[Any]) <- resp if k2 == "results";
            resAny                     <- ress) yield {
        
        def get(Key: String): List[String] = resAny match {
          case tpls:List[(String,String)] => tpls.collect({ case (Key,v) => v })
          case _ => Nil
        }
        
        val url     = get("unescapedUrl").head
        val title   = get("titleNoFormatting").head
        val snippet = get("content").map( _.noTags.noEntities ).head
        
        GoogleResult(new URL(url), title, snippet, Map(), this)
      }
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

  case class Query(cseID: String, query: String, page: Int = 1) extends GoogleQuery {
    def params: String = "alt=atom&prettyprint=true&safe=off"
    def limit: Int = 10
    
    def url: URL = {
      val urlBuilder = new StringBuilder( BASE_URL )
      urlBuilder append "?key="   append API_KEY
      urlBuilder append "&cx="    append cseID
      urlBuilder append "&"       append params
      urlBuilder append "&num="   append limit
      urlBuilder append "&start=" append (page + (page-1)*limit)
      urlBuilder append "&q="     append query.urlEncode
      
      new URL( urlBuilder.toString )
    }

    def process(in: InputStream): List[GoogleResult] = {
      import XOM._
      
      val builder = new Builder()
      val doc = builder build in
      
      val results = for (entry <- doc.getRootElement.getChildElements("entry", NS_ATOM)) yield {
  
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
        
        // collect data-objects
        val data = for (pageMap    <- entry.getChildElements("PageMap", NS_CSE);
                        dataObject <- pageMap.getChildElements("DataObject", NS_CSE)) yield {
  
          // type
          val dataType = dataObject.getAttributeValue("type")
          
          // collect attributes
          val attribs = new scala.collection.mutable.HashMap[String,String]
          for (dataAttr <- dataObject.getChildElements("Attribute", NS_CSE)) {
            val attrName = dataAttr.getAttributeValue("name")
            val attrVal  = dataAttr.getAttributeValue("value")
            attribs.put(attrName, attrVal)
          }
          
          (dataType -> GooglePageMapData(dataType, attribs.toMap))
        }
        
        GoogleResult(new URL(url), title, snippet, data.toMap, this)
      }
      
      results.toList
    }
  }
}




// http://code.google.com/intl/de-DE/apis/customsearch/docs/structured_data.html#pagemaps
case class GooglePageMapData(dataType: String, data: Map[String,String]) {
  def names: Iterable[String] = data.keys
  def get(name: String): Option[String] = data.get(name)
}

case class GoogleResult(url: URL, title: String, snippet: String, pageMap: Map[String, GooglePageMapData], query: GoogleQuery)



// FOCKIN HELL, no results
/*object Google {
  val BASE_URL = "http://www.google.com/search"
  
  case class Query(query: String, extra: String = "") extends GoogleQuery {
    
    // http://code.google.com/intl/de/apis/searchappliance/documentation/46/xml_reference.html#request_parameters
    // http://googlesystem.blogspot.com/2007/04/how-to-disable-google-personalized.html
    def params: String = "ie=utf8&oe=utf8&filter=0&safe=0&pws=0&complete=0&instant=off&hl=en&lr=lang_en|lang_de"
    
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")
      
      val urlBuilder = new StringBuilder( Google.BASE_URL )
      urlBuilder append "?"   append params
      urlBuilder append "&q=" append q append extra
            
      new URL( urlBuilder.toString )
    }
    
    def parser = parse _
  }
    
  def parse(q: GoogleQuery, in: InputStream): List[GoogleResult] = {
    val tagsoup = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser")
    val builder = new Builder( tagsoup )
    val doc = builder build in
    
    println( doc.toXML )
    
    Nil
  }
}*/