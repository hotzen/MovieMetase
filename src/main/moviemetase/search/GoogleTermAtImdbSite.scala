package moviemetase
package search

import Util._
import sites._
import scala.collection.mutable.ListBuffer
import moviemetase.sites.GoogleResult

case class GoogleTermAtImdbSite(term: String, filter: Option[GoogleResult => Boolean] = None) extends Task[List[Movie]] with Logging {
  val logID = "GoogleTermAtImdbSite"
  
  def query = term + " site:imdb.com/title/"
  
  val Max = 3
    
  def execute(): List[Movie] = {
    val results  = Google.Query(query).execute()
    
    val filtered = results.filter( filterResult(_) )
    if (filtered.isEmpty)
      Nil
    else
      processResults( filtered )
  }
 
  def filterResult(res: GoogleResult): Boolean = filter match {
    case Some(f) => f(res)
    case None    => true
  }
    
  def processResults(results: List[GoogleResult]): List[Movie] = {
    
    val imdbUrls = results.flatMap( res => IMDB.extractTitleUrls( res.url.toString ) )

    if (imdbUrls.isEmpty) {
      trace("No IMDB-URLs found")
      return Nil 
    }

    val candidates = imdbUrls.distinctWithCount()

    if (isLogging(LogLevel.Trace))
      for (url <- candidates)
        trace("IMDB: '" + url._1 + "' (" + url._2 + ")")
    
    if (candidates.length > Max)
      warn("More than " + Max + " IMDB-URLs found, restricting to top " + Max)

    val outcome = candidates.take( Max ).map(_._1)
    
    val imdbIDs = {
      for (url <- outcome)
        yield IMDB.extractID(url)
    }.flatten
    
    val fetching =
      for (id <- imdbIDs)
        yield IMDB.FetchByID(id).submit()
            
    fetching.flatMap( fut => fut.get() )
  }
}