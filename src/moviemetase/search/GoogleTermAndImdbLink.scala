package moviemetase
package search

import Util._
import query._
//import java.util.concurrent.Future
import scala.collection.mutable.ListBuffer

class GoogleTermAndImdbLink extends SearchStrategy[Movie] with Logging {
  
  val logID = "GoogleTermAndImdbLink"

  def search(term: String): List[Movie] = {
    
    // search at google for the term linking to IMDB
    val q = '"' + term + '"' + " link:imdb.com/title/"
    trace("querying GoogleAjax with '" + q + "' ...")
    
    val googleQry = GoogleAjax.Query(q)
    val googleFut = googleQry.execute()
    val googleRes = googleFut.get() // block
    
    val urls = googleRes.flatMap(r => IMDB.extractTitleUrls( r.snippet ) )
    val distinctUrls = urls.distinctCount()
    
    if (urls.isEmpty)
      return Nil
    
    if (isLogging(LogLevel.Trace))
      for (url <- distinctUrls)
        trace("found " + url._1 + " (" + url._2 + ")")
        
    val firstUrl = urls.head
    val mostUrl  = distinctUrls.head._1
    val mostCnt  = distinctUrls.head._2

    {
      if (firstUrl == mostUrl || mostCnt == 1) {
        trace("IMDB-Lookup for IMDB-URL: " + firstUrl)
        imdbLookup(firstUrl, 0.95) :: Nil
      } else {
        trace("IMDB-Lookups for first " + firstUrl + " and most-occurred IMDB-URL " + mostUrl)
        imdbLookup(firstUrl, 0.7) :: imdbLookup(mostUrl, 0.7)  :: Nil
      }
    }.flatten
  }
  
  def imdbLookup(url: String, baseScore: Double): Option[Movie] = {
    val lookup = GoogleCSE.Query(IMDB.CSE, url)
    val fut = lookup.execute()
    val res = fut.get() // block
    //println( res.mkString("\n") )
    
    val infos = new ListBuffer[MovieInfo]
    
    //for (r <- res if r.url.toString.contains( url )) {
    for (r <- res if r.url.toString == url) {
      infos append MovieInfos.Score( baseScore )
            
      infos append MovieInfos.Imdb( IMDB.extractId(r.url.toString).head )

      for (dataObject <- r.pageMap) {
        //println( dataObject.toString )
        infos appendAll extractInfosFromGooglePageMap( dataObject )
      }
    }
    
    println( infos )
    
    val movieOpt = Movie(infos)
    
    if (movieOpt.isEmpty) {
      warn("could not extract enough information!")
    }
    
    movieOpt
  }
  
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