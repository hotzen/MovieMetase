package moviemetase
package test

import sites._
import search._

object OpenSubtitlesImdb {
  def main(args: Array[String]) = {
    
    val t = OpenSubtitles.Login().execute()
    println(t)
    
    val imdb = MovieInfos.IMDB("tt0088247")
    val r = OpenSubtitles.IMDB(t.get, imdb).execute()
    println( r.mkString("\n") )
    
    ()
  }
}