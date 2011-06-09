package moviemetase

import java.util.concurrent.Callable
import java.net.URL
import nu.xom._
import org.xml.sax.helpers.XMLReaderFactory


object Google {
  
  def parse(q: GoogleQuery, doc: Document): List[GoogleResult] = {
    Nil
  }
  
  case class Query(query: String) extends GoogleQuery {
    def url = new URL("http://foobar.net")
    def isCSE = false
  }
}

object GoogleCSE {
  val API_KEY  = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
  val BASE_URL = "https://www.googleapis.com/customsearch/v1"
    
  val NS_ATOM = "http://www.w3.org/2005/Atom"
  val NS_CSE  = "http://schemas.google.com/cseapi/2010"
  val NS_OS   = "http://a9.com/-/spec/opensearch/1.1/"
  val NS_GD   = "http://schemas.google.com/g/2005"

  def parse(q: GoogleQuery, doc: Document): List[GoogleResult] = {
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
  
  case class Query(query: String, cse: String, page: Int = 1) extends GoogleQuery {
    def isCSE = true
    
    def params: String = "alt=atom&safe=off&prettyprint=true"
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
  }
}


sealed trait GoogleQuery {
  def query: String
  def url: URL
  def isCSE: Boolean
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
    conn.connect
    
    val in = conn.getInputStream
    
    val builder = if (q.isCSE) new Builder()
                  else         new Builder( XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser") )
        
    val doc = builder build in
    
    if (q.isCSE) GoogleCSE.parse(q, doc)
    else         Google.parse(q, doc)
  }
}