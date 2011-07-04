package moviemetase
package test

import search._
import Util._

import java.net.URL
import scala.collection.mutable.ListBuffer

object ImdbLink {
  def main(args: Array[String]) {
    
    val title = "Inception.1080p.BluRay.x264-REFiNED"
    val q = GoogleAjax.Query(title + " link:imdb.com/title/")
    val s = q.task()
    val rs = s.call()
    
    // search all snippets for an IMDB-link
    val ImdbRegex = """imdb.com/title/tt[0-9]+""".r
    val imdbUrls = rs.flatMap(r => ImdbRegex.findFirstIn( r.snippet ) ).map( m => "http://www." + m + "/")
    println("found " + imdbUrls.length + " IMDB-links")
    
    // remove duplicates, count occurrence in tuple _1, sort by occurrence descending
    def pack(ls: List[String], ps: List[(Int,String)] = Nil): List[(Int,String)] = ls match {
      case Nil   => ps
      case x::xs => pack( xs.remove(_ == x), (1 + xs.count(_ == x), x) :: ps )
    }
    val imdbPackedUrls = pack( imdbUrls ).sort( (t1,t2) => t1._1 > t2._1 )
    println("IMDB-Links found: " + imdbPackedUrls)
    
    // use first only
    println("using imdb-link: " + imdbPackedUrls.head)
    val imdbUrl = imdbPackedUrls.head._2
    
    // collect infos
    val infos = new ListBuffer[MovieInfo]( )
        
    infos appendAll queryIMDB( imdbUrl )
//    infos appendAll queryTMDB( )
    
    val m = Movie.create( infos.toList )
    println( m )
    
    ()
  }
  
  val IMDB_CSE = "011282045967305256347:dyc6spozqnc"
  
  def queryIMDB(imdbUrl: String): List[MovieInfo] = {
    println("----------")
    println("CSE-Search at IMDB for: " + imdbUrl)
    
    val q = GoogleCSE.Query(IMDB_CSE, imdbUrl)
    val s = q.task()
    val rs = s.call()
    
    //println( rs.mkString("\n") )
    val PathRegex = """/title/tt[0-9]+""".r
    val infos = new ListBuffer[MovieInfo]
    
    for (r <- rs) {
      println("CSE-Result: " + r.url)
      infos append MovieInfos.IMDB( r.url.toString )
      
      for (dataType <- r.pageMap.keys) {
        println("PageMap " + dataType)
        infos appendAll parseImdbPageMap(dataType, r.pageMap.get(dataType).head)
      }
    }
    
    infos.toList
  }
  
  def queryTMDB(imdbID: String) {
    
  }
//  
//  def transformIMDB(pageMap: GooglePageMap): Movie = {
//    
//    null
//  }
  
  def parseImdbPageMap(pageMapDataType: String, pageMapData: GooglePageMapData): List[MovieInfo] = {
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