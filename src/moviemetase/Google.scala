package moviemetase

import java.util.concurrent.Callable
import java.net.URL

object GoogleCSE {
  val APIKey  = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
  val BaseURL = "https://www.googleapis.com/customsearch/v1"
}


sealed trait GoogleQuery {
  def cseID: String
  def cseParams: String = "num=10&alt=atom&safe=off&prettyprint=true"
  
  def query: String
  
  def genURL: URL = {
    val q = java.net.URLEncoder.encode(query, "UTF-8")
    
    val urlBuilder = new StringBuilder( GoogleCSE.BaseURL )
    urlBuilder append "?key=" append GoogleCSE.APIKey
    urlBuilder append "&cx="  append cseID
    urlBuilder append "&"     append cseParams
    urlBuilder append "&q="   append q
    
    new URL( urlBuilder.toString )
  }
}

object GoogleQuery {
  case class Subtitles(query: String) extends GoogleQuery {
    def cseID = "011282045967305256347:bmstxyqkjim"
  }
  
  case class IMDB(query: String) extends GoogleQuery {
    def cseID = "011282045967305256347:dyc6spozqnc"    
  }
}

case class GoogleResult(url: String, title: String, snippet: String)

case class GoogleSearch(q: GoogleQuery) extends Callable[List[GoogleResult]] {
  def call(): List[GoogleResult] = {
    
    val url = q.genURL
    
    println("GoogleSearch.call URL=" + url)
    
    val conn = url.openConnection()
    conn setUseCaches false
    conn setAllowUserInteraction false
    conn setDoInput true
    conn setDoOutput false
    conn.connect
    
    val doc = scala.xml.XML.load( conn.getInputStream )
    
    println( doc.toString )
    
//    { for (entry <- doc \ "entry") yield
//        GoogleResult(
//          (entry \ "link" \ "@href").head.text,
//          //Util.removeHTML( (entry \ "title").head.text ),
//          (entry \ "title").head.text,
//          //Util.removeHTML( (entry \ "summary").head.text )
//          (entry \ "summary").head.text
//        )
//    }.toList
    Nil
  }
}