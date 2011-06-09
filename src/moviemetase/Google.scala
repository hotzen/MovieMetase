package moviemetase

import java.util.concurrent.Callable
import java.net.URL
import nu.xom._
import org.xml.sax.helpers.XMLReaderFactory
import java.io.InputStream
import java.io.BufferedReader
import scala.io.Source
import scala.util.parsing.json.JSON


// FUCKING HELL, no results
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

object GoogleAjax {
  val BASE_URL = "http://ajax.googleapis.com/ajax/services/search/web"
  
  case class Query(query: String, page: Int = 1) extends GoogleQuery {
    def params: String = "v=1.0&rsz=large&hl=en"
    def limit: Int = 8
      
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")
      
      val urlBuilder = new StringBuilder( GoogleAjax.BASE_URL )
      urlBuilder append "?"   append params
      urlBuilder append "&start=" append ((page-1)*limit)
      urlBuilder append "&q=" append q
      
      // do not escape extra-query, but replace spaces
      //if (extraQuery.length > 0)
      //  urlBuilder append extraQuery.replace(" ", "+")
      
      new URL( urlBuilder.toString )
    }
    
    def parser = parse _
  }
    
  def parse(q: GoogleQuery, in: InputStream): List[GoogleResult] = {
    
    val s = Source.fromInputStream(in).mkString
    
    println( s )
    
    JSON.parse( s ) match {
      case None => println("NIX")
      case c@Some(elems) => println( c.mkString("\n") ) 
    }
    
    Nil
  }
}



object GoogleCSE {
  val API_KEY  = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
  val BASE_URL = "https://www.googleapis.com/customsearch/v1"
    
  val NS_ATOM = "http://www.w3.org/2005/Atom"
  val NS_CSE  = "http://schemas.google.com/cseapi/2010"
  val NS_OS   = "http://a9.com/-/spec/opensearch/1.1/"
  val NS_GD   = "http://schemas.google.com/g/2005"

  case class Query(query: String, cse: String, page: Int = 1) extends GoogleQuery {
    def params: String = "alt=atom&prettyprint=true&safe=off"
    def limit: Int = 10
    
    def url: URL = {
      val q = java.net.URLEncoder.encode(query, "UTF-8")
      
      val urlBuilder = new StringBuilder( GoogleCSE.BASE_URL )
      urlBuilder append "?key="   append GoogleCSE.API_KEY
      urlBuilder append "&cx="    append cse
      urlBuilder append "&"       append params
      urlBuilder append "&num="   append limit
      urlBuilder append "&start=" append (page + (page-1)*limit)
      urlBuilder append "&q="     append q
      
      new URL( urlBuilder.toString )
    }

    def parser = parse _
  }
    
  def parse(q: GoogleQuery, in: InputStream): List[GoogleResult] = {

    val builder = new Builder()
    val doc = builder build in
    
    def removeTags(s: String) = """</?.*?>""".r.replaceAllIn(s, "")
    def removeEntities(s: String) = """&[a-z]+;""".r.replaceAllIn(s, "")
    
    import XOM._
    
    val results = for (entry <- doc.getRootElement.getChildElements("entry", NS_ATOM)) yield {

      // <link href="http://www.imdb.com/title/tt0088247/" title="www.imdb.com" />
      val url = entry.
        getChildElements("link", NS_ATOM).
        map( _.getAttributeValue("href") ).
        map( new URL(_) ).
        head // XXX might fail
        
      // <title type="html">The &lt;b&gt;Terminator&lt;/b&gt; (1984) - IMDb</title>
      val title = entry.
        getChildElements("title", NS_ATOM).
        map( _.getValue ).
        map( removeTags(_) ).
        map( removeEntities(_) ).
        head // XXX might fail
      
      // <summary type="html">Cast/credits plus other information about the film.</summary>
      val snippet = entry.
        getChildElements("summary", NS_ATOM).
        map( _.getValue ).
        map( removeTags(_) ).
        map( removeEntities(_) ).
        head // XXX might fail
      
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
      
      GoogleResult(url, title, snippet, GooglePageMap(data.toMap), q)
    }
    
    results.toList
  }
  
  
}


sealed trait GoogleQuery {
  def query: String
  def url: URL

  def parser: ( (GoogleQuery, InputStream) => List[GoogleResult] )
}


// http://code.google.com/intl/de-DE/apis/customsearch/docs/structured_data.html#pagemaps
case class GooglePageMap(data: Map[String, GooglePageMapData]) {
  def dataTypes: Iterable[String] = data.keys
  def get(dataType: String): Option[GooglePageMapData] = data.get(dataType)
}
case class GooglePageMapData(dataType: String, data: Map[String,String]) {
  def names: Iterable[String] = data.keys
  def get(name: String): Option[String] = data.get(name)
}

case class GoogleResult(url: URL, title: String, snippet: String, pageMap: GooglePageMap, query: GoogleQuery)


case class GoogleSearch(q: GoogleQuery) extends Callable[List[GoogleResult]] {
  def call(): List[GoogleResult] = {
    
    val conn = q.url.openConnection()
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput true
    conn setDoOutput false
  
    //conn setRequestProperty ("Accept", "text/xml;q=0.9,application/xml;q=0.8,application/xhtml+xml;q=0.7,text/html;q=0.3,text/*;q=0.1")
    //conn setRequestProperty ("Accept-Charset", "utf-8;q=0.9,ISO-8859-1;q=0.5,*;q=0.1")
    conn setRequestProperty ("Accept-Encoding", "identity")
    //conn setRequestProperty ("Accept-Language", "en-US;q=0.9,de-DE;q=0.5")
    
    conn setRequestProperty ("Referer", "http://stackoverflow.com/questions/tagged/google")
    conn setRequestProperty ("User-Agent", "Mozilla/5.0") // (Windows NT 6.1; WOW64) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.91 Safari/534.30") 
    
    conn.connect
    
    val parse = q.parser
    parse( q, conn.getInputStream )
  }
}