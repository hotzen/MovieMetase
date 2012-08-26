package moviemetase.test

import moviemetase.MovieInfos
import moviemetase.extraction._
import moviemetase.App

object ExtractorFileTest {

  def main(args: Array[String]): Unit = {
    val f = new java.io.File( args(0) )
    val in = args(1)
   
    val s = scala.io.Source.fromFile(f)("UTF-8")
    val cnt = s.getLines.mkString("\n")
   
    try {
      val extrs = DSL.apply( cnt )
      for (extr <- extrs) {
        println(extr.toString)
        
        val res = extr.execute( in )
        for (x <- res) {
          println(x)
        }
        println("=============================")
      }
    } finally {
      App.shutdown
    }
  }
}