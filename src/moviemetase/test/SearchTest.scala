package moviemetase
package test

import search.MovieSearch

object SearchTest {
  def main(args: Array[String]) = {
    
    val ms = new MovieSearch
    
    val paths = if (!args.isEmpty) {
      val f = new java.io.File( args(0) )
      scala.io.Source.fromFile(f, "utf-8").getLines.map(_.trim).filter(!_.startsWith("#")).toList
    } else {
      "V:\\01_Filme\\Wolfman.UNRATED.1080p.BluRay.x264-REFiNED\\refined-wolfman-1080p.mkv" :: Nil
    }
    
    for (path <- paths) {
      println("================================================")
      try {
        val res = ms.searchByFile( FileInfo(path) )
        println( res.map(_.toString).mkString("\n") )
      } catch {
        case e:Exception => {
          println("FAIL " + path)
          e.printStackTrace()
        }
      }
      
    }
    
    println("DONE, shutting down...")
    TaskExecutor.shutdown()
  }
}