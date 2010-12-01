//package moviemetase
//
//import java.util.concurrent.Callable
//import java.net.URL
//
//object Google {
//  val APIKey = "AIzaSyAeeLMANIJTh5H2aZTusm1_iUyudRMQABc"
//  val BaseURL = "https://www.googleapis.com/customsearch/v1?key=" + APIKey + "&num=10&alt=atom&safe=off&prettyprint=true"
//}
//
//object GoogleCSE {
//  // http://www.google.com/cse/panel/sites?cx=011282045967305256347:ere22gz0l-w&sig=__owv3JFsJ7skOA4UJNXvsl8cSpTQ=
//  
//  val MainSites = "011282045967305256347:uxnvlsnwltc" // IMDB + TMDB
//  val IMDB = "011282045967305256347:dyc6spozqnc"
//  val TMDB = "011282045967305256347:6gzcajg3evm"
//  val Subtitles = "011282045967305256347:bmstxyqkjim"
//  
//  //val Movies = "011282045967305256347:ere22gz0l-w"
//    
//  //val IMDB = "011282045967305256347:dyc6spozqnc"
//  //val TMDB = "011282045967305256347:6gzcajg3evm"
//}
//
//sealed trait GoogleQuery {
//  def queryParam: String
//  def extraParams: String = ""
//  def genURL: URL = new URL( Google.BaseURL + "&q=" + queryParam + extraParams )
//}
//
//object GoogleQuery {
//  // NICHT MEHR MOEGLICH MIT CUSTOM GOOGLE SEARCH
////  case class Query(term: String) extends GoogleQuery {
////    def queryParam = Util.URLEncode(term)
////  }
////  case class Site(term: String, site: String) extends GoogleQuery {
////    def queryParam = Util.URLEncode(term) + "+site:" + Util.URLEncode(site)
////  }
////  case class Related(url: String) extends GoogleQuery {
////    def queryParam = "related:" + Util.URLEncode("url")
////  }
//  case class Scoped(term: String, CSE: String) extends GoogleQuery {
//    def queryParam = java.net.URLEncoder.encode(term, "UTF-8")
//    override def extraParams = "&cx=" + CSE
//  }
//}
//
//case class GoogleResult(url: String, title: String, snippet: String)
//
//case class GoogleSearch(q: GoogleQuery) extends Callable[List[GoogleResult]] {
//  def call(): List[GoogleResult] = {
//    
//    val url = q.genURL
//    
//    println("GoogleSearch.call URL=" + url)
//    
//    val conn = url.openConnection()
//    conn setUseCaches false
//    conn setAllowUserInteraction false
//    conn setDoInput true
//    conn setDoOutput false
//    conn.connect
//    
//    val doc = scala.xml.XML.load( conn.getInputStream )
//
//    { for (entry <- doc \ "entry") yield
//        GoogleResult(
//          (entry \ "link" \ "@href").head.text,
//          //Util.removeHTML( (entry \ "title").head.text ),
//          (entry \ "title").head.text,
//          //Util.removeHTML( (entry \ "summary").head.text )
//          (entry \ "summary").head.text
//        )
//    }.toList
//  }
//}