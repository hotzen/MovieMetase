package moviemetase
package search

import Util._
import sites._
import scala.collection.mutable.ListBuffer

case class GoogleTermAtImdbSite(term: String, requiresMatch: Option[GoogleResult => Boolean] = None) extends Task[List[Movie]] with Logging {
  val logID = "GoogleTermAtImdbSite"
  
  def query = term + " site:imdb.com/title/"
  
  val Max = 3
  
  final def execute(): List[Movie] = {
    val googleRes = Google.Query(query).execute()
    
    val imdbTitleUrls = googleRes.filter(res => requiresMatch match {
      case Some(f) => f(res)
      case None    => true
    }).flatMap(r => {
      IMDB.extractTitleUrls( r.url.toString )
    })

    if (imdbTitleUrls.isEmpty) {
      trace("Nothing found")
      return Nil 
    }
        
    val distinctTitleUrls = imdbTitleUrls.distinctWithCount()

    if (isLogging(LogLevel.Trace))
      for (url <- distinctTitleUrls)
        trace("IMDB URL: '" + url._1 + "' (" + url._2 + ")")
    
    if (distinctTitleUrls.length > Max)
      warn("More than " + Max + " IMDB-URLs found, restricting to the top " + Max)

    val tryUrls = distinctTitleUrls.take( Max ).map(_._1)
    
    for (url <- tryUrls) {
      trace("using: " + url)
    }

    // IMDB-Title-URL => IMDB-ID
    val tryIds =
      for (url <- tryUrls)
        yield IMDB.extractId(url).head
        
    // fetch
    val futs =
      for (id <- tryIds)
        yield IMDB.FetchByID(id).submit()

    // join
    futs.flatMap( _.get() )
  }
}