package moviemetase

import java.net.URI

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

case class Movie(title: String, year: Int, infos: List[MovieInfo] = Nil)

sealed trait MovieInfo {
  var origin: Option[String] = None
}

object MovieInfos {
  case class Genre(name: String) extends MovieInfo
  case class Plot(text: String) extends MovieInfo
  
  case class AlternativeTitle(title: String) extends MovieInfo
  case class Rating(rating: Double, max: Double) extends MovieInfo
      
  case class Actor(name: String, character: String) extends MovieInfo
  case class Director(name: String) extends MovieInfo
  case class Producer(name: String) extends MovieInfo
  case class Writer(name: String)   extends MovieInfo
    
  case class Page(url: String) extends MovieInfo
  case class Trailer(url: String) extends MovieInfo
  case class Subtitle(url: String, lang: String) extends MovieInfo
  
  case class Poster(url: String) extends MovieInfo
  case class Backdrop(url: String) extends MovieInfo
  
  case class Extra(name: String, value: String) extends MovieInfo
}