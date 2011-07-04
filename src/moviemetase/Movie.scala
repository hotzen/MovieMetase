package moviemetase

import java.net.URL
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
      val infoStr = info.toString.grouped(150).mkString("\n    ")
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
    def url: URL
    
    import java.io.File
    import java.util.concurrent.Callable
    
    def createDownloader(target: File) = new Callable[(URL,File)] {
      import java.net.URL
      import java.io._
      import java.nio._
      import java.nio.channels._
          
      def call(): (URL, File) = {
        val rbc: ReadableByteChannel = Channels.newChannel( url.openStream() )
        val fos: FileOutputStream    = new FileOutputStream( target )
        fos.getChannel().transferFrom(rbc, 0, 1 << 24)
        (url, target)
      }
    }
  }
  
  trait Image extends MovieInfo with Downloadable {
    def url: URL
    def preview: Option[URL]
  }
  
  trait Website extends MovieInfo {
    def url: URL
  }
  
  
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
    
  case class IMDB(id: String) extends MovieInfo
  case class TMDB(id: String) extends MovieInfo
  
  case class ImdbSite(url: URL) extends MovieInfo with Website
  case class TmdbSite(url: URL) extends MovieInfo with Website
    
  case class Trailer(label: String, url: URL) extends MovieInfo with Website
  case class Subtitle(label: String, lang: String, url: URL) extends MovieInfo with Downloadable
  
  case class Poster(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  case class Backdrop(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  
  case class Extra(name: String, value: String) extends MovieInfo
}