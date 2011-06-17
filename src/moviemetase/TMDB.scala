package moviemetase

import Util._
import java.net.URL
import java.io.InputStream
import nu.xom._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

sealed trait TmdbQuery extends Query[Movie]

object TMDB {
  
  case class ImdbLookup(imdbID: String) extends TmdbQuery {
    val BASE_URL = "http://api.themoviedb.org/2.1/Movie.imdbLookup/en/xml/"
    val API_KEY  =  "be11971c1e1d73be3465ce33a0aaed78"
    
    def query: String = imdbID
      
    def url: URL = {
      val urlBuilder = new StringBuilder( BASE_URL )
      urlBuilder append API_KEY append "/" append imdbID
      
      new URL( urlBuilder.toString )
    }
    
    def parse(in: InputStream): List[Movie] = {
      import XOM._
      
      val builder = new Builder()
      val doc = builder build in
            
      val infos = new ListBuffer[MovieInfo]()
      
      val movies = for (elemMovies <- doc.getRootElement.getChildElements("movies");
                        elemMovie  <- elemMovies.getChildElements("movie") ) yield {
        
        infos.clear
        
        infos append MovieInfos.TMDB(
          elemMovie.getChildElements("url").
            map( _.getValue ).
            head
        )
        
        infos append MovieInfos.Title(
          elemMovie.getChildElements("original_name").
            map( _.getValue ).
            head
        )
        
        infos append MovieInfos.AlternativeTitle(
          elemMovie.getChildElements("alternative_name").
            map( _.getValue ).
            head
        )
        
        infos append MovieInfos.Release(
          elemMovie.getChildElements("released").
            map( _.getValue ).
            head.split("-")(0).toInt
        )
        
        infos append MovieInfos.Summary(
          elemMovie.getChildElements("overview").
            map( _.getValue ).
            head
        )
        
        for (elemCats <- elemMovie.getChildElements("categories");
             elemCat  <- elemCats.getChildElements("category") if elemCat.getAttributeValue("type") == "genre") {
          
          infos append MovieInfos.Genre(
            elemCat.getAttributeValue("name")
          )
        }
        
        case class Image(id: String, url: String, size: String, imgType: String)
        val imgMap = new HashMap[String,List[Image]]
        
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
        
        for ( (_,imgs) <- imgMap) {
          
          val img     = imgs.find(img => img.size == "original").head
          val preview = imgs.find(img => img.size == "cover" || img.size == "poster")
                    
          if (img.imgType == "poster")
            infos append MovieInfos.Poster(img.url, preview.map(p => p.url))    
          else if (img.imgType == "backdrop")
            infos append MovieInfos.Backdrop(img.url, preview.map(p => p.url))
          else
            assert(false, "unknown TMDB image-type: " + img.imgType)
        }
                
        Movie.fromInfos( infos ).head
      }
      
      movies.toList
    }
  }
}
