package moviemetase
package search

import Util._
import sites._
import scala.collection.mutable.ListBuffer

case class GoogleTermWithImdbLink(term: String, requiresMatch: Option[GoogleResult => Boolean] = None) extends Task[List[Movie]] with Logging {
  val logID = "GoogleTermWithImdbLink"
  
  val query = term + " linkto:imdb.com/title/"
  
  final def execute(): List[Movie] = {
    val googleRes = GoogleAjax.Query(query).execute()

    for (res <- googleRes) {
      trace(res.toString)
    }
    
    val imdbTitleUrls = googleRes.flatMap(extractImdbUrl(_))

    if (imdbTitleUrls.isEmpty) {
      trace("Nothing found")
      return Nil 
    }

    val distinctTitleUrls = imdbTitleUrls.distinctWithCount()

    if (isLogging(LogLevel.Trace))
      for (url <- distinctTitleUrls)
        trace("IMDB URL: '" + url._1 + "' (" + url._2 + ")")
        
    val firstUrl = imdbTitleUrls.head
    val mostUrl  = distinctTitleUrls.head._1
    val mostCnt  = distinctTitleUrls.head._2
    
    // Use only the first URL if it is also the most-occurring one,
    // otherwise use the first and the most-occurring URL
    val tryUrls =
      if (firstUrl == mostUrl || mostCnt == 1)
        firstUrl :: Nil
      else
        firstUrl :: mostUrl :: Nil
    
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
  
  def extractImdbUrl(res: GoogleResult): Option[String] = {
    val matched =
      requiresMatch match {
        case Some(matchFn) => matchFn(res)
        case None          => true
      }
    
    if (!matched)
      None
    else
      IMDB.extractTitleUrls( WhitespaceRegex.replaceAllIn(res.snippet.toLowerCase, "") ).headOption
  }
}