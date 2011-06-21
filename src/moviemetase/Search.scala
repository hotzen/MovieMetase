package moviemetase
import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream

case class MovieSearch(val out: PrintStream = System.out) extends SearchSupervisor[Movie] {
    
  def searchTerm(term: String): List[(Double,Movie)] = {
    val s = new TermWithImdbLinkSearch("FileNameWithImdbLink") with TmdbIntegrator
    s.out = out
    
    val fut = s.execute( term )
    val res = fut.get()
    
    filter(res)
  }
  
  def searchByFile(fileInfo: FileInfo): List[(Double,Movie)] = {
    
    val fut1 = {
      val t = fileInfo.fileName
      val s = new TermWithImdbLinkSearch("FileNameWithImdbLink") with TmdbIntegrator
      s.out = out
      
      s.execute( t )
    }
    
    val fut2 = {
      val t = fileInfo.dirName
      val s = new TermWithImdbLinkSearch("DirNameWithImdbLink") with TmdbIntegrator
      s.out = out
      
      s.execute( t )
    }
    
    val res = {
      fut1.get() :: fut2.get() :: Nil
    }
    
    filter(res.flatten)
  }
}

abstract class SearchSupervisor[A] {
  type ScoredResult = (Double, A)

  def filter(res: List[ScoredResult]): List[ScoredResult] = res.sortWith( (t1,t2) => t1._1 > t2._1 ).take(3)
  
  def searchTerm(term: String): List[ScoredResult]
  def searchByFile(fileInfo: FileInfo): List[ScoredResult]
}


abstract class Search[A] {
  var out: PrintStream = System.out
  
  def id: String
  def execute(term: String): Future[List[(Double,A)]] // Tuple(Score,Result)

  def trace(s: String): Unit =
    out.println("[TRACE] {" + id + "} " + s)
}

object IMDB {
  val CSE  = "011282045967305256347:dyc6spozqnc"
    
  val UrlRegex  = """imdb.com/title/tt[0-9]+""".r
  val PathRegex = """/title/tt[0-9]+""".r
  val IdRegex   = """tt[0-9]+""".r
}

class TermWithImdbLinkSearch(val id: String) extends Search[Movie] {
  
  def execute(term: String): Future[List[(Double,Movie)]] =
    WorkerPool submit { search(term) }
  
  def search(term: String): List[(Double,Movie)] = {
    
    val q = term + " link:imdb.com/title/"
    trace("querying GoogleAjax with '" + q + "' ...")
    
    // search at google for the term linking to IMDB
    val googleQry = GoogleAjax.Query(q)
    val googleFut = googleQry.execute()
    val googleRes = googleFut.get() // block
    
    // extract IMDB-URLs
    val imdbUrls = googleRes.flatMap(r => IMDB.UrlRegex.findFirstIn( r.snippet ) ).map( m => "http://www." + m + "/")
    
    // group by URL, count occurrence, sort by occurrence
    val imdbPackedUrls = imdbUrls.packed.sort( (t1,t2) => t1._1 > t2._1 )
    trace("found " + imdbPackedUrls.length + " distinct IMDB-URLs")
    
    // nothing found, abort
    if (imdbUrls.isEmpty)
      return Nil
    
    // get first found URL and the URL with most occurrence
    val firstImdbUrl = imdbUrls.head
    val mostImdbCnt  = imdbPackedUrls.head._1
    val mostImdbUrl  = imdbPackedUrls.head._2
    
    {
      if (firstImdbUrl == mostImdbUrl || mostImdbCnt == 1) {
        trace("IMDB-Lookup for first/most IMDB-URL: " + firstImdbUrl)
        imdbLookup(firstImdbUrl, 0.95) :: Nil
      } else {
        trace("IMDB-Lookups for first " + firstImdbUrl + " and most IMDB-URL " + mostImdbUrl)
        imdbLookup(firstImdbUrl, 0.7) :: imdbLookup(mostImdbUrl, 0.7)  :: Nil
      }
        
    }.flatten
  }
  
  def imdbLookup(imdbUrl: String, score: Double): Option[(Double,Movie)] = {
    val lookup = GoogleCSE.Query(IMDB.CSE, imdbUrl)
    val fut = lookup.execute()
    val res = fut.get() // block
    
    val infos = new ListBuffer[MovieInfo]
    
    for (r <- res if r.url.toString.contains( imdbUrl )) {
      infos append MovieInfos.IMDB( r.url.toString )
      
      for (dataType <- r.pageMap.keys) {
        trace("Parsing Google PageMap of IMDB-Lookup Result: " + r.url + " ("+score+")")
        infos appendAll processGooglePageMap(dataType, r.pageMap.get(dataType).head)
      }
    }
    
    Movie.create(infos) match {
      case Some(movie) => {
        trace("sucessfully created: " + movie)
        Some(score,movie)
      }
      case None => {
        trace("NO MOVIE CREATED, not enough information: " + infos.mkString(", "))
        None
      }
    }
  }
  
  def processGooglePageMap(pageMapDataType: String, pageMapData: GooglePageMapData): List[MovieInfo] = {
    val infos = new ListBuffer[MovieInfo]
    
    pageMapDataType.toLowerCase match {
      // MOVIE
      case "movie" => {
        pageMapData.get("image") match {
          case Some(url) => infos append MovieInfos.Poster( url )
          case _ =>
        }
        pageMapData.get("director") match {
          case Some(name) => infos append MovieInfos.Director( name.trim )
          case _ =>
        }
        pageMapData.get("title") match {
          case Some(t) => {
            val TitleRegex = """(.+?)\(([0-9]+)\)""".r
            
            TitleRegex.findFirstMatchIn( t ) match {
              case Some(m) => {
                infos append MovieInfos.Title( m.group(1).trim )
                infos append MovieInfos.Release( m.group(2).trim.toInt )
              }
              case None => {
                infos append MovieInfos.Title( t.trim )
              }
            }
          }
          case _ =>
        }
      }
      // REVIEW
      case "moviereview" => {
        pageMapData.get("genre") match {
          case Some(gs) => {
            for (g <- gs.split("/")) {
              infos append MovieInfos.Genre( g.trim )
            }
          }
          case _ =>
        }
        pageMapData.get("starring") match {
          case Some(actors) => {
            for (actor <- actors.split(",")) {
              infos append MovieInfos.Actor( actor.trim )
            }
          }
          case _ =>
        }
        pageMapData.get("image_href") match {
          case Some(url) => infos append MovieInfos.Poster( url )
          case _ =>
        }
        pageMapData.get("originalrating") match {
          case Some(r) => infos append MovieInfos.ImdbRating( r.trim.toDouble )
          case _ =>
        }
//        pageMapData.get("summary") match {
//          case Some(s) => infos append MovieInfos.Summary( s.trim )
//          case _ =>
//        }
      }
      // IMAGE
      case "image" => {
        pageMapData.get("src") match {
          case Some(url) => infos append MovieInfos.Poster( url )
          case _ =>
        }
      }
      case x => {
        //prtln("Unknown PageMap DataType '" + x + "'")
      }
    }
   
    infos.toList
  }
}


trait TmdbIntegrator extends Search[Movie] { // self: Search[Movie] =>
  abstract override def execute(term: String): Future[List[(Double,Movie)]] = WorkerPool submit {
      
    // execute actual search
    val fut = super.execute(term)
    val res = fut.get() // block
    
    // do TMDB-lookups for each result
    val resFut = for ( (score, movie) <- res) yield {
      val infos = movie.infos
      
      // search for IMDB-IDs
      val ids = infos.collect({ case MovieInfos.IMDB(url) => IMDB.IdRegex.findFirstIn(url) }).flatten.
                  packed.sortWith( (t1,t2) => t1._1 > t2._1 )
      
      if (ids.isEmpty) {
        trace("TmdbIntegrator found no IMDB-ID, skipping " + movie)
        (score, movie, None)
      } else {
        val imdbID = ids.head._2
        val fut = TMDB.ImdbLookup(imdbID).execute()
        (score, movie, Some(fut) )
      }
    }
    
    // join futures and integrate their result-infos
    for ( (score, movie, futOpt) <- resFut) yield futOpt match {
      case Some(fut) => {
        val tmdbMovies = fut.get() // blocking wait
        
        if (tmdbMovies.isEmpty) {
          trace("TMDB IMDB-Lookup returned NO movies, aborting augmentation")
          (score, movie)
        
        } else {
          if (!tmdbMovies.tail.isEmpty)
            trace("TMDB IMDB-Lookup returned " + tmdbMovies.length + " movies, using only the first")
          
          val tmdbMovie = tmdbMovies.head
          val tmdbInfos = tmdbMovie.infos
          
          // filter out existing title+release, trust TMDB
          val filtInfos = movie.infos.filter( _ match {
            case MovieInfos.Title(_)   => false
            case MovieInfos.Release(_) => false
            case _                     => true
          })
          
          val newMovie = Movie.create(filtInfos ::: tmdbInfos).head
          trace("successfully integrated TMDB-Information")
          (score, newMovie)
        }
      }
      case None => (score, movie)
    }
  }
}