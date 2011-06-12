package moviemetase
package test
import java.net.URL

object SearchTest {
  def main(args: Array[String]) {
    
    val title = "Inception.1080p.BluRay.x264-REFiNED"
    val q = GoogleAjax.Query(title + " link:imdb.com/title/")
    val s = GoogleSearch(q)
    val rs = s.call()
    
    val ImdbRegex = """imdb.com/title/tt[0-9]+""".r
          
    val urls = rs.flatMap(r => ImdbRegex.findFirstIn( r.snippet ) )
      .map( m => "http://www." + m + "/")
    
    println("found " + urls.length + " IMDB-links")
      
    // remove duplicates, return tuple where _1 == occurance
    def pack(ls: List[String], ps: List[(Int,String)] = Nil): List[(Int,String)] = ls match {
      case Nil   => ps
      case x::xs => pack( xs.remove(_ == x), (1+xs.count(_ == x), x) :: ps )
    }
    val purls = pack( urls )
      .sort( (t1,t2) => t1._1 < t2._1 )
      .map(t => t._2)
      
    println("found " + purls.length + " different IMDB-links: " + purls.mkString(", "))
    
    val ms = purls.map(queryIMDB(_))
          
    ()
  }
  
  val IMDB_CSE = "011282045967305256347:dyc6spozqnc"
  
  def queryIMDB(url: String): Option[Movie] = {
    println("----------")
    println("CSE-Search at IMDB for: " + url)
    
    val q = GoogleCSE.Query(IMDB_CSE, url)
    val s = GoogleSearch(q)
    val rs = s.call()
    println( rs.mkString("\n") )
    
    val PathRegex = """/title/tt[0-9]+$""".r
    
    val infos = for (r <- rs; dataType <- r.pageMap.dataTypes) yield {
      println("Checking CSE-PageMap of Result " + r.url)
      
      val pageMapData = r.pageMap.get(dataType).head
      
      dataType.toLowerCase match {
        case "movie" => {
          println("PageMap Movie")
                    
        }
        case "moviereview" => {
          println("PageMap MovieReview")
         
        }
        case "image" => {
          println("PageMap Image")
          
        }
        case x => {
          println("Unknown PageMap DataType '" + x + "'")
          
        }
      }
      
      for ((k,v) <- pageMapData.data) {
        println("[ " + k + " ] " + v)
      }
      
      println("--")
  
      
      
      // TODO
      MovieInfos.Title(r.title)
    }
    
    Movie.fromInfos(infos)
  }
  
  def transformIMDB(pageMap: GooglePageMap): Movie = {
    
    null
  }
}