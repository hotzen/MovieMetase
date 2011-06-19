package moviemetase

import java.net.URI
import java.util.Date


object Movie {
  def create(infos: Traversable[MovieInfo]): Option[Movie] = {
    val t = infos.collect({ case MovieInfos.Title(t) => t   })   
    val y = infos.collect({ case MovieInfos.Release(d) => d })
    
    if (t.isEmpty)
      return None
    
    val title = t.head
    val year  = if (y.isEmpty) 0 else y.head
    
    Some( Movie(title, year, infos.toList.distinct) )
  }
}

case class Movie(title: String, year: Int, infos: List[MovieInfo] = Nil) {
  override def toString: String = {
    val s = new StringBuffer
    s append "Movie(_" append title append "_/" append year append "){\n"
    for (info <- infos) {
      s append "  " append info.toString append "\n"
    }
    s append "}"
    s.toString
  }
}

sealed trait MovieInfo {
  var origin: String = ""
}


object MovieInfos {
  case class Title(name: String) extends MovieInfo
  case class Release(year: Int) extends MovieInfo
  
  case class Genre(name: String) extends MovieInfo
  
  case class Summary(text: String) extends MovieInfo
  case class Plot(text: String) extends MovieInfo
  
  case class AlternativeTitle(title: String) extends MovieInfo
  //case class Rating(rating: Double, max: Double) extends MovieInfo
  case class ImdbRating(rating: Double) extends MovieInfo
  
  case class Actor(name: String)    extends MovieInfo
  case class Director(name: String) extends MovieInfo
  case class Producer(name: String) extends MovieInfo
  case class Writer(name: String)   extends MovieInfo
    
  case class IMDB(url: String) extends MovieInfo
  case class TMDB(url: String) extends MovieInfo
  //case class Page(url: String) extends MovieInfo
  case class Trailer(url: String) extends MovieInfo
  case class Subtitle(url: String, lang: String) extends MovieInfo
  
  //case class Thumbnail(url: String) extends MovieInfo
  //case class SmallPoster(url: String) extends MovieInfo
  case class Poster(url: String, preview: Option[String] = None) extends MovieInfo
  case class Backdrop(url: String, preview: Option[String] = None) extends MovieInfo
  
  case class Extra(name: String, value: String) extends MovieInfo
}