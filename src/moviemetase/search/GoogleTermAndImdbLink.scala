package moviemetase
package search

import Util._
import query._
//import java.util.concurrent.Future
import scala.collection.mutable.ListBuffer

case class GoogleTermAndImdbLink(term: String) extends Task[List[Movie]] with Logging {
  
  val logID = "GoogleTermAndImdbLink"

  def execute(): List[Movie] = {
    val q = GoogleQuery().strict(term).linkTo("imdb.com/title/").toString
    
    trace("Querying GoogleAjax: '" + q + "' ...")
    val googleRes = GoogleAjax.Query(q).execute()
    
    val imdbTitleUrls = googleRes.flatMap(r => IMDB.extractTitleUrls( r.snippet ) )
    val distinctTitleUrls = imdbTitleUrls.distinctWithCount()
            
    if (imdbTitleUrls.isEmpty)
      return Nil
      
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
    val tryIds = for (url <- tryUrls) yield IMDB.extractId(url).head
    
    // Futures of extraction
    val futs =
      for (id <- tryIds)
        yield IMDB.ExtractInfos(id).submit() 

    // Extracted Infos
    val extracts =
      for (fut <- futs)
        yield fut.get()
            
    // Create Movies from Infos
    val movies =
      for (infos <- extracts) yield {
        val movie = Movie(infos)
        info( movie.toString )
        movie
      }
    
    movies.flatten.toList
  }
  
//  def imdbLookup(url: String, baseScore: Double): Option[Movie] = {
//    val res = GoogleCSE.Query(IMDB.CSE, url).execute()
//    //println( res.mkString("\n") )
//    
//    val infos = new ListBuffer[MovieInfo]
//    
//    //for (r <- res if r.url.toString.contains( url )) {
//    for (r <- res if r.url.toString == url) {
//      infos append MovieInfos.Score( baseScore )
//      
//      IMDB.extractId(r.url.toString).foreach( infos append MovieInfos.IMDB(_) )
//      //infos append MovieInfos.IMDB( IMDB.extractId(r.url.toString).head )
//
//      for (dataObject <- r.pageMap) {
//        //println( dataObject.toString )
//        infos appendAll extractInfosFromGooglePageMap( dataObject )
//      }
//    }
//    
//    println( infos )
//    
//    val movieOpt = Movie(infos)
//    
//    if (movieOpt.isEmpty) {
//      warn("could not extract enough information!")
//    }
//    
//    movieOpt
//  }
  
  def extractInfosFromGooglePageMap(dataObject: GooglePageMapDataObject): List[MovieInfo] = {
    val infos = new ListBuffer[MovieInfo]
    
    dataObject.dataType.toLowerCase match {
      // MOVIE
      case "movie" => {
        dataObject.get("image") match {
          case Some(url) => infos append MovieInfos.Poster( url.toURL )
          case _ =>
        }
        dataObject.get("director") match {
          case Some(name) => infos append MovieInfos.Director( name.trim )
          case _ =>
        }
        dataObject.get("title") match {
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
        dataObject.get("genre") match {
          case Some(gs) => {
            for (g <- gs.split("/")) {
              infos append MovieInfos.Genre( g.trim )
            }
          }
          case _ =>
        }
        dataObject.get("starring") match {
          case Some(actors) => {
            for (actor <- actors.split(",")) {
              infos append MovieInfos.Actor( actor.trim )
            }
          }
          case _ =>
        }
        dataObject.get("image_href") match {
          case Some(url) => infos append MovieInfos.Poster( url.toURL )
          case _ =>
        }
        dataObject.get("originalrating") match {
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
        dataObject.get("src") match {
          case Some(url) => infos append MovieInfos.Poster( url.toURL )
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