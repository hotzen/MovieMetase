package moviemetase

import java.util.concurrent.Callable
import java.net.URL
import nu.xom._

object GoogleCSE {
  val APIKey  = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
  val BaseURL = "https://www.googleapis.com/customsearch/v1"
    
  val NS_ATOM = "http://www.w3.org/2005/Atom"
  val NS_CSE  = "http://schemas.google.com/cseapi/2010"
  val NS_OS   = "http://a9.com/-/spec/opensearch/1.1/"
  val NS_GD   = "http://schemas.google.com/g/2005"
   
  val ns = List[(String,String)](
        ("atom" -> NS_ATOM)
      , ("cse"  -> NS_CSE)
      , ("os"   -> NS_OS)
      , ("gd"   -> NS_GD)
  )
  
  def baseURL = "http://www.w3.org/2005/Atom"
  
  def ctx: XPathContext = {
    val ctx = new XPathContext
    for ((prefix,uri) <- ns)
      ctx.addNamespace(prefix, uri)
    ctx
  }
}


sealed trait GoogleQuery {
  def cseID: String
  def params: String = "alt=atom&safe=off&prettyprint=true"
  
  def query: String
  
  def limit: Int = 10
  def page:  Int = 1
  
  def url: URL = {
    val q = java.net.URLEncoder.encode(query, "UTF-8")
    
    val urlBuilder = new StringBuilder( GoogleCSE.BaseURL )
    urlBuilder append "?key="   append GoogleCSE.APIKey
    urlBuilder append "&cx="    append cseID
    urlBuilder append "&"       append params
    urlBuilder append "&num="   append limit
    urlBuilder append "&start=" append (page + (page-1)*limit)
    urlBuilder append "&q="     append q
    
    new URL( urlBuilder.toString )
  }
}

object GoogleQuery {
  case class Custom(query: String, cseID: String)
  
  case class IMDB(query: String) extends GoogleQuery {
    def cseID = "011282045967305256347:dyc6spozqnc"
  }
}

case class GoogleDataObject(dataType: String, data: Map[String,String]) {
  def names: Iterable[String] = data.keys
  def value(name: String): Option[String] = data.get(name)
}

case class GoogleResult(url: URL, title: String, snippet: String, data: Map[String, GoogleDataObject], query: GoogleQuery)

case class GoogleSearch(q: GoogleQuery) extends Callable[List[GoogleResult]] {
  def call(): List[GoogleResult] = {
    val url = q.url

    val conn = url.openConnection()
    conn setUseCaches true
    conn setAllowUserInteraction false
    conn setDoInput true
    conn setDoOutput false
    conn.connect
    val in = conn.getInputStream
    
    import XOM._
    val builder = new nu.xom.Builder
    val doc     = builder.build(in)
    val root    = doc.getRootElement
    
    //println( doc.toXML )
    
    def removeTags(s: String) = """</?.*?>""".r.replaceAllIn(s, "")
    def removeEntities(s: String) = """&[a-z]+;""".r.replaceAllIn(s, "")
    
    val results = for (entry <- root.getChildElements("entry", GoogleCSE.NS_ATOM)) yield {

      // <link href="http://www.imdb.com/title/tt0088247/" title="www.imdb.com" />
      val url = entry.
        getChildElements("link", GoogleCSE.NS_ATOM).
        map( _.getAttributeValue("href") ).
        map( new URL(_) ).
        head // XXX might fail
        
      // <title type="html">The &lt;b&gt;Terminator&lt;/b&gt; (1984) - IMDb</title>
      val title = entry.
        getChildElements("title", GoogleCSE.NS_ATOM).
        map( _.getValue ).
        map( removeTags(_) ).
        map( removeEntities(_) ).
        head // XXX might fail
      
      // <summary type="html">Cast/credits plus other information about the film.</summary>
      val snippet = entry.
        getChildElements("summary", GoogleCSE.NS_ATOM).
        map( _.getValue ).
        map( removeTags(_) ).
        map( removeEntities(_) ).
        head // XXX might fail
      
      // collect data-objects
      val data = for (pageMap    <- entry.getChildElements("PageMap", GoogleCSE.NS_CSE);
                      dataObject <- pageMap.getChildElements("DataObject", GoogleCSE.NS_CSE)) yield {

        // type
        val dataType = dataObject.getAttributeValue("type")
        
        // collect attributes
        val attribs = new scala.collection.mutable.HashMap[String,String]
        for (dataAttr <- dataObject.getChildElements("Attribute", GoogleCSE.NS_CSE)) {
          val attrName = dataAttr.getAttributeValue("name")
          val attrVal  = dataAttr.getAttributeValue("value")
          attribs.put(attrName, attrVal)
        }
        
        (dataType -> GoogleDataObject(dataType, attribs.toMap))
      }
      
      GoogleResult(url, title, snippet, data.toMap, q)
    }
    
    results.toList
  }
}