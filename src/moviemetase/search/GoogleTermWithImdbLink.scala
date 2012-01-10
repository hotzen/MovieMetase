package moviemetase
package search

import Util._
import sites._
import scala.collection.mutable.ListBuffer

case class GoogleTermWithImdbLink(term: String, requiresMatch: Option[GoogleResult => Boolean] = None) extends Task[List[Movie]] with Logging {
  val logID = "GoogleTermWithImdbLink"
  
  val query = term + " linkto:imdb.com/title/"
  
  final def execute(): List[Movie] = {
    val googleRes = Google.Query(query).execute()
    
    val imdbTitleUrls = googleRes.filter(res => requiresMatch match {
      case Some(f) => f(res)
      case None    => true
    }).flatMap(res => {
      val snippet = WhitespaceRegex.replaceAllIn(res.snippet.toLowerCase, "")
      IMDB.extractTitleUrls(snippet).headOption
    })
    
    if (imdbTitleUrls.isEmpty) {
      trace("Nothing found")
      return Nil 
    }

    val totalUrls = imdbTitleUrls.length
    val distinctUrls = imdbTitleUrls.distinctWithCount()

    if (isLogging(LogLevel.Trace))
      for (url <- distinctUrls)
        trace("IMDB URL: '" + url._1 + "' (" + url._2 + ")")
    
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

    val tryUrls = distinctUrls.flatMap( dis => {
      val (url, count) = dis
      val ratio = count.toFloat / totalUrls
      if (ratio > 0.35) {
        trace("using " + url + " (" + count + ") good ratio " + ratio)
        Some(url)
      } else {
        trace("ignoring " + url + " (" + count + ") bad ratio " + ratio)
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
        yield IMDB.extractId(url).head
    
    // fetch
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