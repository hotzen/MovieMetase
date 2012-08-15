package moviemetase

import java.net.URL
import scala.collection.mutable.Set
import scala.concurrent.Future

object Movie {
  
  // create a Movie from MovieInfos
  def apply(infos: Traversable[MovieInfo]): Option[Movie] = {
    var newInfos = new scala.collection.mutable.ListBuffer[MovieInfo]
    newInfos appendAll infos.toList.distinct
    
    // try to parse TitleWithRelease Infos
    val (optTitleInfo, optReleaseInfo) = parseAllTitleWithReleases(
      infos.collect({ case MovieInfos.TitleWithRelease(tr) => tr }).toList
    )
    for (titleInfo <- optTitleInfo)
      newInfos append titleInfo
    for (releaseInfo <- optReleaseInfo)
      newInfos append releaseInfo

    // create Movie from Title and Release
    val t = newInfos.collect({ case MovieInfos.Title(t)   => t }).headOption
    val r = newInfos.collect({ case MovieInfos.Release(d) => d }).headOption
    
    if (!t.isDefined)
      return None
    
    val title = t.head
    val year  = if (r.isEmpty) 0 else r.head
    
    Some( Movie(title, year, newInfos.distinct.sortWith(_.order < _.order).toList) )
  }
  
  def parseAllTitleWithReleases(trs: List[String]): (Option[MovieInfos.Title], Option[MovieInfos.Release]) = {
    if (trs.isEmpty)
      return (None, None)
    
    for (tr <- trs) {
       parseTitleWithRelease(tr) match {
         case Some((titleInfo, releaseInfo)) =>
           return (Some(titleInfo), Some(releaseInfo))
         case None =>
       }
    } 
    
    // use the first as title, dont specify any release-year
    (Some(MovieInfos.Title(trs.head)), None)
  }

  def parseTitleWithRelease(tr: String): Option[(MovieInfos.Title, MovieInfos.Release)] =
    """^(.+?)[\(\[ ]?([0-9]+)[\)\] ]?$""".r.findFirstMatchIn(tr) match {
      case Some(m) => {
        val title = m.group(1).trim
        val year  = m.group(2).toInt
        Some(MovieInfos.Title(title), MovieInfos.Release(year))
      }
      case None => None
    }
}

case class Movie(title: String, year: Int, infos: List[MovieInfo] = Nil) {
  
  def id: String = hashCode.toString //title + "@" + year
  
  def label: String = title + "/" + year
    
  def withNewInfos(infos2: List[MovieInfo]): Movie =
    Movie(title, year, (infos ::: infos2).distinct)
    
  override def toString: String = {
    val sb = new StringBuffer
    sb append "Movie("
    sb append title
    sb append "/"
    sb append year
    sb append " [Infos="
    sb append infos.length
    sb append "])"
    sb.toString
  }
    
  def toStringWithInfos: String = {
    val sb = new StringBuffer
    sb append "Movie("
    sb append title
    sb append "/"
    sb append year
    sb append "){\n"
    for (info <- infos) {
      val infoStr = info.toString.grouped(150).map(_.trim).mkString("\n    ")
      sb append "  " append infoStr append "\n"
    }
    sb append "}"
    sb.toString
  }
}

trait MovieInfo {
  def order: Int
  def id: String = hashCode.toString
  
  var source: String = ""
  def withSourceInfo(info: String): this.type = {
    source = info
    this
  }
}


object MovieInfos {
  case class TitleWithRelease(titleRelease: String) extends MovieInfo { val order = 1 }
  
  case class Title(name: String) extends MovieInfo { val order = 2 }
  case class Release(year: Int) extends MovieInfo { val order = 3 }
  
  case class AlternativeTitle(title: String) extends MovieInfo { val order = 4 }
  
  case class Genre(name: String) extends MovieInfo { val order = 10 }
  
  case class Actor(name: String, character: Option[String] = None) extends MovieInfo { val order = 20 }
  case class Director(name: String) extends MovieInfo { val order = 21 }
  case class Producer(name: String) extends MovieInfo { val order = 22 }
  case class Writer(name: String)   extends MovieInfo { val order = 23 }
  
  case class Tagline(text: String) extends MovieInfo { val order = 30 }
  case class Summary(text: String) extends MovieInfo { val order = 31 }
    
  case class IMDB(imdbID: String, rating: Option[Float] = None) extends MovieInfo with Website {
    val order: Int = 80
    lazy val page: URL = new URL( "http://www.imdb.com/title/" + imdbID + "/" )
  }
  
  case class TMDB(tmdbID: String, rating: Option[Float] = None) extends MovieInfo with Website {
    val order: Int = 81
    lazy val page: URL = new URL( "http://www.themoviedb.org/movie/" + tmdbID )
  }
  
  case class Thumbnail(url: URL) extends MovieInfo with Image {
    val order = 50
    val preview = Some(url)
  }
  
  case class Poster(url: URL, preview: Option[URL] = None) extends MovieInfo with Image { val order = 51 }
  case class Backdrop(url: URL, preview: Option[URL] = None) extends MovieInfo with Image { val order = 52 }
  
  case class Subtitle(label: String, lang: String, page: URL, url: URL) extends MovieInfo with Website with Downloadable { val order = 60 }  
  case class Trailer(label: String, page: URL) extends MovieInfo with Website { val order = 70 }
    
  case class Extra(name: String, value: String) extends MovieInfo { val order = 100 }
    
  
  trait Downloadable extends MovieInfo {
    import java.io.File
        
    def url: URL
    
    def downloader(to: File): Task[(URL,File)] = DownloadTask(url, to)
  }
    
  
  trait Image extends MovieInfo with Downloadable {
    def url: URL
    def preview: Option[URL]
    
    def file = url // Downloadable
  }
  
  trait Website extends MovieInfo {
    def page: URL
  }
}