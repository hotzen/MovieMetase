package moviemetase
package search

import Util._
import sites._
import scala.collection.mutable.ListBuffer

case class GoogleTermWithImdbLink(term: String, filter: Option[GoogleResult => Boolean] = None) extends Task[List[Movie]] with Logging {
  val logID = "GoogleTermWithImdbLink"
  
  val query = term + " link:imdb.com/title/"
    
  def execute(): List[Movie] = {
    val results  = Google.Query(query).execute()
    
    val filtered = results.filter( filterResult(_) )
    if (filtered.isEmpty)
      return Nil
    
    processResults( filtered )
  }
  
  def filterResult(res: GoogleResult): Boolean = filter match {
    case Some(f) => f(res)
    case None    => true
  }
  
  def processResults(results: List[GoogleResult]): List[Movie] = {
        
    val imdbUrls = results.flatMap(res => {
      val snippet = WhitespaceRegex.replaceAllIn(res.snippet.toLowerCase, "")
      IMDB.extractTitleUrls(snippet).headOption
    })
    
    if (imdbUrls.isEmpty) {
      trace("No IMDB-Links found")
      return Nil 
    }

    val totalUrls = imdbUrls.length
    
    val candidates = imdbUrls.distinctWithCount()

    if (isLogging(LogLevel.Trace)) {
      val t = candidates.length
      for ( (url,i) <- candidates.zipWithIndex) {
        trace("IMDB-Page ("+(i+1)+"/"+t+"): '" + url._1 + "' (found " + url._2 + " times)")
      }
    }
        
    
//    if (candidates.length > Max)
//      warn("More than " + Max + " IMDB-URLs found, restricting to top " + Max)

    
    //val firstUrl   = imdbTitleUrls.head
    //val firstCount = distinctTitleUrls.find(dis => dis._1 == firstUrl).get._2
    //val mostUrl   = distinctUrls.head._1
    //val mostCount = distinctTitleUrls.head._2
    
//    val tryUrls = {
//      val set = scala.collection.mutable.LinkedHashSet[String]() // preserve insertion-order
//      set += mostUrl
//
//      //set += firstUrl
//      
//      set ++= distinctTitleUrls.flatMap( dis => {
//        val (url, count) = dis
//        val ratio = count.toFloat / totalUrls
//        if (ratio > 0.3)
//          Some(url)
//        else {
//          trace("ignoring url " + url + " (" + count + ") due to low ratio of " + ratio)
//          None
//        }
//      })
//      
//      set.toList.reverse
//    }

    val tryUrls = candidates.flatMap( dis => {
      val (url, count) = dis
      val ratio = count.toFloat / totalUrls
      if (ratio > 0.35) {
        trace("PostFilter OK: " + url + " (" + count + ") good ratio " + ratio)
        Some(url)
      } else {
        trace("PostFilter Ignore: " + url + " (" + count + ") bad ratio " + ratio)
        None
      }
    })

    // Use only the first URL if it is also the most-occurring one,
    // otherwise use the first and the most-occurring URL
//    val tryUrls =
//      if (firstUrl == mostUrl || mostCnt == 1)
//        firstUrl :: Nil
//      else
//        firstUrl :: mostUrl :: Nil
    
    // IMDB-Title-URL => IMDB-ID
    val tryIds =
      for (url <- tryUrls)
        yield IMDB.extractID(url).head
            
    val futs =
      for (id <- tryIds)
        yield IMDB.FetchByID(id).submit()
        
//    val futs =
//      for (id <- tryIds) yield {
//        IMDB.FetchByID(id).thenFork(_ match {
//          case Some(movie) => TMDB.AutoExpandMovie(movie) :: Nil
//          case None        => Nil
//        }).submit()
//      }

    // join
    futs.flatMap( _.get() )
  }
  
  val WhitespaceRegex = """\s""".r
}