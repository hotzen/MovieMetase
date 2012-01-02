package moviemetase

import java.net.URL

object Movie {
  
  // create a Movie from MovieInfos
  def apply(infos: Traversable[MovieInfo]): Option[Movie] = {
    var newInfos = new scala.collection.mutable.ListBuffer[MovieInfo]
    newInfos appendAll infos.toList.distinct
    
    val (optTitleInfo, optReleaseInfo) = parseAllTitleWithReleases(
      infos.collect({ case MovieInfos.TitleWithRelease(tr) => tr }).toList
    )
    
    for (titleInfo <- optTitleInfo)
      newInfos append titleInfo
      
    for (releaseInfo <- optReleaseInfo)
      newInfos append releaseInfo

    val t  = newInfos.collect({ case MovieInfos.Title(t)   => t  }).headOption
    val r  = newInfos.collect({ case MovieInfos.Release(d) => d  }).headOption
    
    if (t.isDefined) {
      val title = t.head
      val year  = if (r.isEmpty) 0 else r.head
      Some( Movie(title, year, newInfos.toList) )
    } else None
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
  override def toString: String = {
    val s = new StringBuffer
    s append "Movie(" append title append "/" append year append "){\n"
    for (info <- infos) {
      val infoStr = info.toString.grouped(150).map(_.trim).mkString("\n    ")
      s append "  " append infoStr append "\n"
    }
    s append "}"
    s.toString
  }
}

sealed trait MovieInfo {
  var source: String = ""
  
  def withSourceInfo(info: String): this.type = {
    source = info
    this
  }
}


object MovieInfos {
  trait Downloadable extends MovieInfo {
    
    // which file to download
    def file: URL
    
    import java.io.File
    import java.util.concurrent.Callable
    
    def downloadTask(target: File) = new Callable[(URL,File)] {
      import java.io.FileOutputStream
      import java.nio.channels.{Channels, ReadableByteChannel}
      //import java.nio.channels._
          
      def call(): (URL, File) = {
        val rbc: ReadableByteChannel = Channels.newChannel( file.openStream() )
        val fos: FileOutputStream = new FileOutputStream( target )
        val fch = fos.getChannel()
        fch.lock()
        fch.transferFrom(rbc, 0, 1 << 24)
        (file, target)
      }
    }
  }
  
  trait Image extends MovieInfo with Downloadable {
    def url: URL
    def preview: Option[URL]
    
    def file = url // Downloadable
  }
  
  trait WebPage extends MovieInfo {
    def page: URL
  }
  
  // search scoring
  case class Score(score: Double) extends MovieInfo
  
  case class TitleWithRelease(titleRelease: String) extends MovieInfo
  
  case class Title(name: String) extends MovieInfo
  case class Release(year: Int) extends MovieInfo
  
  case class Genre(name: String) extends MovieInfo
  
  case class Description(text: String) extends MovieInfo
  case class Summary(text: String) extends MovieInfo
  //case class Plot(text: String) extends MovieInfo
  
  case class AlternativeTitle(title: String) extends MovieInfo
  //case class Rating(rating: Double, max: Double) extends MovieInfo
  case class ImdbRating(rating: Double) extends MovieInfo
  
  case class Actor(name: String, character: Option[String] = None) extends MovieInfo
  case class Director(name: String) extends MovieInfo
  case class Producer(name: String) extends MovieInfo
  case class Writer(name: String)   extends MovieInfo
    
  case class IMDB(id: String) extends MovieInfo with WebPage {
    lazy val page: URL = new URL( "http://www.imdb.com/title/" + id + "/" )
  }
  
  case class TMDB(id: String) extends MovieInfo with WebPage {
    lazy val page: URL = new URL( "http://www.themoviedb.org/movie/" + id )
  }
    
  case class Trailer(label: String, page: URL) extends MovieInfo with WebPage
  case class Subtitle(label: String, lang: String, page: URL, file: URL) extends MovieInfo with WebPage with Downloadable 
  
  case class Thumbnail(url: URL) extends MovieInfo with Image {
    val preview = Some(url)
  }
  
  case class Poster(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  case class Backdrop(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  
  case class Extra(name: String, value: String) extends MovieInfo
}