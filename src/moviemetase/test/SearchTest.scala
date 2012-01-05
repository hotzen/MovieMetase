package moviemetase
package test

import search.MovieSearch

object SearchTest {
  def main(args: Array[String]) = {
    
    val ms = new MovieSearch
    
    if (!args.isEmpty) {
      val f = new java.io.File( args(0) )
      val lines = scala.io.Source.fromFile(f, "utf-8").getLines.toList
      
      for (line <- lines) {
        println("================================================")
        val res = ms.searchByFile( FileInfo(line) )
        println( res.map(_.toString).mkString("\n") )
      }
    } else {
//      println("================================================")
//      val res = ms.searchByTerm( "Inception 2010" )
//      println( res.map(_.toString).mkString("\n") )
      
      println("================================================")
      val res2 = ms.searchByFile( FileInfo("V:\\01_Filme\\Wolfman.UNRATED.1080p.BluRay.x264-REFiNED\\refined-wolfman-1080p.mkv") )
      println( res2.map(_.toString).mkString("\n") )
    }
    
    println("DONE, shutting down...")
    TaskExecutor.shutdown()
  }
}