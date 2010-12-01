package moviemetase

import java.net.URI

object File {
  def fromPath(path: String): File = {
    val f = new java.io.File(path);
    File(path, f.getParent, f.getName)
  }
}
case class File(path: String, dirName: String, fileName: String)

case class Splitted(names: List[String], tags: List[String], year: Option[Int]) {
  def name: String = names.mkString(" ")
  
  override def toString: String = {
    "Splitted(" +
    "names[" + names.mkString(", ") + "] " +
    "tags[" + tags.mkString(", ") + "] " +
    "year=" + year + ")"
  }
}

object Movie {
  import nu.xom._
  import XOM._
  
  def createFromXML(elem: Element): Movie = {
    if (elem.name != "movie")
      throw new Exception("invalid movie Element: " + elem.toXML)
    
    val title = elem.findChild(_.name == "title") match {
      case Some(child) => child.text.trim
      case None => throw new Exception("no title Element")
    }
    
    val year = elem.findChild(_.name == "year") match {
      case Some(child) => child.text.trim.toInt
      case None => -1
    }
    
    val m = Movie(title, year)
    
    m
  }
}

case class Movie(title: String, year: Int, infos: List[MovieInfo] = Nil)

sealed trait MovieInfo {
  var origin: Option[String] = None
}

object MovieInfo {
  import nu.xom._
  import XOM._
  
  def createFromXML(elem: Element, origin: Option[String] = None): MovieInfo = {
    import MovieInfos._
    
    val n = elem.name
    val v = elem.text.trim
    
    val info: MovieInfo = n match {
      case "genre" => Genre(v)
      case "plot"  => Plot(v)
      case "alternative-title" => AlternativeTitle(v)
      case "rating" => {
        val max = elem.attribute("max").getOrElse("-1").toDouble
        Rating(v.toDouble, max)
      }
      
      case "actor" => {
        val char = elem.attribute("character").getOrElse("")
        Actor(v, char)
      }
      case "director" => Director(v)
      case "producer" => Producer(v)
      case "writer"   => Writer(v)
      
      case "page"     => Page(v)
      case "trailer"  => Trailer(v)
      case "subtitle" => {
        val lang = elem.attribute("language").getOrElse("")
        Subtitle(v, lang)
      }
      case "poster"   => Poster(v)
      case "backdrop" => Backdrop(v)
      
      case "_" => Extra(n, v)
    }
    info.origin = origin
    info
  }
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