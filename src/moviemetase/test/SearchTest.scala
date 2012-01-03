package moviemetase
package test

import search.MovieSearch

object SearchTest {
  def main(args: Array[String]) = {
    
    val ms  = new MovieSearch
    
    val res = ms.searchByTerm( "Inception 2010 1080p refined" )
    
    println("====================================")
    println("DONE.")
    println( res.map(_.toStringWithInfos).mkString("\n") )
    ()
  }
}