package moviemetase

import java.net.URL
import java.util.Date


object Movie {
  
  // create a Movie from MovieInfos
  def apply(infos: Traversable[MovieInfo]): Option[Movie] = {
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
    s append "Movie(" append title append " [" append year append "]){\n"
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
    
  case class Imdb(id: String) extends MovieInfo with WebPage {
    lazy val page: URL = new URL( "http://www.imdb.com/title/" + id + "/" )
  }
  
  case class Tmdb(id: String) extends MovieInfo with WebPage {
    lazy val page: URL = new URL( "http://www.themoviedb.org/movie/" + id )
  }
    
  case class Trailer(label: String, page: URL) extends MovieInfo with WebPage
  case class Subtitle(label: String, lang: String, page: URL, file: URL) extends MovieInfo with WebPage with Downloadable 
  
  case class Poster(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  case class Backdrop(url: URL, preview: Option[URL] = None) extends MovieInfo with Image
  
  case class Extra(name: String, value: String) extends MovieInfo
}