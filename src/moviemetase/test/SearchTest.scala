package moviemetase
package test
import java.net.URL
import scala.collection.mutable.ListBuffer

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
         
    println(ms.mkString("\n\n"))
    
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
      val infos = new ListBuffer[MovieInfo]
      
      dataType.toLowerCase match {
        // MOVIE
        case "movie" => {
          pageMapData.get("image") match {
            case Some(url) => infos append MovieInfos.Thumbnail( url )
            case _ =>
          }
          pageMapData.get("director") match {
            case Some(name) => infos append MovieInfos.Director( name.trim )
            case _ =>
          }
          pageMapData.get("title") match {
            case Some(t) => {
              val TitleRegex = """(.+?)\(([0-9]+)\)""".r
              
              TitleRegex.findFirstMatchIn( t ) match {
                case Some(m) => {
                  infos append MovieInfos.Title( m.group(1).trim )
                  infos append MovieInfos.Release( m.group(2).trim.toInt )
                }
                case None => {
                  infos append MovieInfos.Title( t.trim )
                }
              }
            }
            case _ =>
          }
        }
        // REVIEW
        case "moviereview" => {
          pageMapData.get("genre") match {
            case Some(gs) => {
              for (g <- gs.split("/")) {
                infos append MovieInfos.Genre( g.trim )
              }
            }
            case _ =>
          }
          pageMapData.get("starring") match {
            case Some(actors) => {
              for (actor <- actors.split(",")) {
                infos append MovieInfos.Actor( actor.trim )
              }
            }
            case _ =>
          }
          pageMapData.get("image_href") match {
            case Some(url) => infos append MovieInfos.SmallPoster( url )
            case _ =>
          }
          pageMapData.get("originalrating") match {
            case Some(r) => infos append MovieInfos.ImdbRating( r.trim.toDouble )
            case _ =>
          }
          pageMapData.get("summary") match {
            case Some(s) => infos append MovieInfos.Summary( s.trim )
            case _ =>
          }
        }
        // IMAGE
        case "image" => {
          pageMapData.get("src") match {
            case Some(url) => infos append MovieInfos.SmallPoster( url )
            case _ =>
          }
        }
        case x => {
          //prtln("Unknown PageMap DataType '" + x + "'")
        }
      }
            
      infos.toList
    }
    
    Movie.fromInfos( infos.flatten )
  }
  
  def transformIMDB(pageMap: GooglePageMap): Movie = {
    
    null
  }
}