package moviemetase
package test

import sites._
import search._
import scala.concurrent.Await
import scala.concurrent.util.Duration

object OpenSubtitlesImdb {
  def main(args: Array[String]) = {
    
    val r1 = OpenSubtitles.Login().execute()
    println(r1)
    
    val imdb = MovieInfos.IMDB("tt0088247")
    val r2 = OpenSubtitles.IMDB(r1.get, imdb).execute()
    println( r2.mkString("\n") )
    
    ()
  }
}