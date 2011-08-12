package moviemetase
package search

import Util._
import java.util.concurrent.Future
import scala.collection.mutable.ListBuffer

object IMDB {
  val CSE  = "011282045967305256347:dyc6spozqnc"
    
  val UrlRegex  = """imdb.com/title/tt[0-9]+""".r
  val PathRegex = """/title/tt[0-9]+""".r
  val IdRegex   = """tt[0-9]+""".r
}


class TermWithImdbLinkSearch(val id: String) extends Search[Movie] with Logging {
  
  val logID = id

  def search(term: String): List[Movie] = {
    
    val q = term + " link:imdb.com/title/"
    trace("querying GoogleAjax with '" + q + "' ...")
    
    // search at google for the term linking to IMDB
    val googleQry = GoogleAjax.Query(q)
    val googleFut = googleQry.execute()
    val googleRes = googleFut.get() // block
    
    // extract IMDB-URLs
    val imdbUrls = googleRes.flatMap(r => IMDB.UrlRegex.findFirstIn( r.snippet ) ).map( m => "http://www." + m + "/")
    
    // group by URL, count occurrence, sort by occurrence
    val imdbPackedUrls = imdbUrls.countDistinct().sortByCount()
    trace("found " + imdbPackedUrls.length + " distinct IMDB-URLs")
    
    // nothing found, abort
    if (imdbUrls.isEmpty)
      return Nil
    
    // get first found URL and the URL with most occurrence
    val firstImdbUrl = imdbUrls.head
    val mostImdbUrl  = imdbPackedUrls.head._1
    val mostImdbCnt  = imdbPackedUrls.head._2

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
  
  def imdbLookup(imdbUrl: String, score: Double): Option[Movie] = {
    val lookup = GoogleCSE.Query(IMDB.CSE, imdbUrl)
    val fut = lookup.execute()
    val res = fut.get() // block
    
    val infos = new ListBuffer[MovieInfo]
    
    for (r <- res if r.url.toString.contains( imdbUrl )) {
      infos append MovieInfos.Score( score )
      infos append MovieInfos.Imdb( r.url.toString )
      
      
      for (dataType <- r.pageMap.keys) {
        trace("Parsing Google PageMap of IMDB-Lookup Result: " + r.url + " ("+score+")")
        infos appendAll processGooglePageMap(dataType, r.pageMap.get(dataType).head)
      }
    }
    
    Movie(infos) match {
      case Some(movie) => {
        trace("sucessfully created: " + movie)
        Some(movie)
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
          case Some(url) => infos append MovieInfos.Poster( url.toURL )
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
          case Some(url) => infos append MovieInfos.Poster( url.toURL )
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