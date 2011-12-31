package moviemetase
package test

import search.MovieSearch

object SearchTest {
  def main(args: Array[String]) = {
    
    val ms  = new MovieSearch
    
    val res = ms.searchByTerm( "Inception.1080p.BluRay.x264-REFiNED" )
    println( res.mkString("\n") )
    ()
  }
}