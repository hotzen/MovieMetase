package moviemetase
package query

import query._
import Util._
import java.net.URL
import java.io.InputStream
import nu.xom._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.util.concurrent.Future

object TMDB {
  val API_KEY = "be11971c1e1d73be3465ce33a0aaed78"
  val API_URL = "http://api.themoviedb.org/2.1/" 
  
  val IdRegex        = """/movie/([0-9]+)""".r
    
  def extractId(s: String): Option[String] = IdRegex.findFirstMatchIn(s) match {
    case Some(m) => Some( m.group(1) )
    case None    => None
  }
      
  case class AutoExpandMovie(movie: Movie) extends Task[Movie] with Logging {
    val logID = "TMDB.AutoExpandMovie(" + movie.label + ")"
    
    def execute(): Movie = {
      
      // try IMDB-ID
      for (imdbID <- movie.infos.collect({ case MovieInfos.IMDB(id) => id }).headOption) {
        trace("expanding by IMDB-ID: " + imdbID)
        for (tmdbMovie <- FetchByImdbID(imdbID).execute())
          return movie.withNewInfos( tmdbMovie.infos ) 
      }
      
      // default return unchanged
      movie
    }
  }
    
  case class FetchByImdbID(imdbID: String) extends XmlTask[Option[Movie]] with Logging {
    val logID = "TMDB.FetchByImdbID(" + imdbID + ")"
    
    def url: URL = {
      val sb = new StringBuilder( API_URL )
      sb append "Movie.imdbLookup/en/xml/"
      sb append API_KEY
      sb append "/" append imdbID
      trace("API-URL: " + sb)
      new URL( sb.toString )
    }
    
    def process(doc: Document): Option[Movie] = {
      import XOM._
      //println(doc.toXML)
      
      val movies =
        for (elemMovies <- doc.getRootElement.getChildElements("movies");
             elemMovie  <- elemMovies.getChildElements("movie").headOption) yield {
          
          val infos = new ListBuffer[MovieInfo]()
          
          elemMovie.getChildElements("url").headOption.
            map( _.getValue() ).
            flatMap( TMDB.extractId(_) ).
            foreach( infos append MovieInfos.TMDB(_) )
          
          elemMovie.getChildElements("original_name").headOption.
            map( _.getValue ).
            foreach( infos append MovieInfos.Title(_) )
            
          elemMovie.getChildElements("alternative_name").headOption.
            map( _.getValue ).
            foreach( infos append MovieInfos.AlternativeTitle(_) )
                    
          elemMovie.getChildElements("released").
            map( _.getValue ).
            map( _.split("-")(0).toInt ).
            foreach( infos append MovieInfos.Release(_) )

          elemMovie.getChildElements("overview").
            map( _.getValue ).
            foreach( infos append MovieInfos.Summary(_) )
                   
          // collect genres
          for (elemCats <- elemMovie.getChildElements("categories");
               elemCat  <- elemCats.getChildElements("category") if elemCat.getAttributeValue("type") == "genre") {
            
            infos append MovieInfos.Genre(
              elemCat.getAttributeValue("name")
            )
          }
          
          case class Image(id: String, url: String, size: String, imgType: String)
          val imgMap = new HashMap[String,List[Image]]
          
          // collect images of different sizes in a map
          for (elemImgs <- elemMovie.getChildElements("images");
               elemImg  <- elemImgs.getChildElements("image")) {
                      
            val imgID   = elemImg.getAttributeValue("id")
            val imgURL  = elemImg.getAttributeValue("url")
            val imgSize = elemImg.getAttributeValue("size")
            val imgType = elemImg.getAttributeValue("type")
            
            val img = Image(imgID, imgURL, imgSize, imgType)
            
            imgMap.get(imgID) match {
              case Some(others) => imgMap += (imgID -> (img :: others)) 
              case None         => imgMap += (imgID -> (img :: Nil))
            }
          }
          
          // process images
          for ( (_,imgs) <- imgMap) {
            
            val img     = imgs.find(img => img.size == "original").head
            val preview = imgs.find(img => img.size == "cover" || img.size == "poster")
                      
            if (img.imgType == "poster")
              infos append MovieInfos.Poster(img.url.toURL, preview.map(p => p.url.toURL))    
            else if (img.imgType == "backdrop")
              infos append MovieInfos.Backdrop(img.url.toURL, preview.map(p => p.url.toURL))
            else
              assert(false, "unknown TMDB image-type: " + img.imgType)
          }
                    
          val movie = Movie(infos)
          trace( movie.toString )
          movie
        }
       
       movies.flatten.headOption
    }
  }
}