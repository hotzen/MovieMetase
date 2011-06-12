package moviemetase

import java.net.URI
import java.util.Date

object FileInfo {
  def fromPath(path: String): FileInfo = {
    val f = new java.io.File(path);
    FileInfo(path, f.getParent, f.getName)
  }
}
case class FileInfo(path: String, dirName: String, fileName: String)

case class Chopped(names: List[String], tags: List[String], year: Option[Int]) {
  def name: String = names.mkString(" ")
  
  override def toString: String = {
    "Chopped(" + name + "; "
    "names[" + names.mkString(", ") + "] " +
    "tags[" + tags.mkString(", ") + "] " +
    "year=" + year + ")"
  }
}

object Movie {
  def fromInfos(infos: List[MovieInfo]): Option[Movie] = {
    val t = infos.collect({ case MovieInfos.Title(t) => t   })   
    val y = infos.collect({ case MovieInfos.Release(d) => d })
    
    if (t.isEmpty)
      return None
    
    val title = t.head
    val year  = if (y.isEmpty) 0 else y.head.getYear
    
    Some( Movie(title, year, infos) )
  }
}

case class Movie(title: String, year: Int, infos: List[MovieInfo] = Nil)

sealed trait MovieInfo
object MovieInfos {
  case class Title(name: String) extends MovieInfo
  case class Release(date: Date) extends MovieInfo
  
  case class Genre(name: String) extends MovieInfo
  case class Plot(text: String) extends MovieInfo
  
  case class AlternativeTitle(title: String) extends MovieInfo
  case class Rating(rating: Double, max: Double) extends MovieInfo
      
  case class Actor(name: String, character: String) extends MovieInfo
  case class Director(name: String) extends MovieInfo
  case class Producer(name: String) extends MovieInfo
  case class Writer(name: String)   extends MovieInfo
    
  case class IMDB(url: String) extends MovieInfo
  case class TMDB(url: String) extends MovieInfo
  case class Page(url: String) extends MovieInfo
  case class Trailer(url: String) extends MovieInfo
  case class Subtitle(url: String, lang: String) extends MovieInfo
  
  case class Poster(url: String) extends MovieInfo
  case class Backdrop(url: String) extends MovieInfo
  
  case class Extra(name: String, value: String) extends MovieInfo
}