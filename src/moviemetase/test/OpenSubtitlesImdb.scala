package moviemetase
package test

import search._

object OpenSubtitlesImdb {
  def main(args: Array[String]) = {
    
    val t = OpenSubtitles.Login().task().call()
    println(t)
    
    val imdb = MovieInfos.IMDB("tt0088247")
    val r = OpenSubtitles.SearchByImdb(t.get, imdb).task().call()
    println( r.mkString("\n") )
    
    ()
  }
}