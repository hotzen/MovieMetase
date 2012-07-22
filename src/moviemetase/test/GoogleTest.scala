package moviemetase
package test

import sites._
import search._
import scala.util.Random
import java.io.PrintWriter

object GoogleTest {
  def main(args: Array[String]) = {
    Logging.out = new PrintWriter(System.out)
    Logging.level = LogLevel.Trace
    
//    { // CSE IMDB Search
//      val title = "Terminator"
//      val IMDB = "011282045967305256347:dyc6spozqnc"
//      println( GoogleCSE.Query(IMDB, title).execute() )
//    }
//    
//    { // Ajax Search
//      val title = "Inception.1080p.BluRay.x264-REFiNED"
//      println( GoogleAjax.Query(title + " link:imdb.com/title/").execute() )
//    }
    
    val rnd = new Random
    for (i <- 1 to 1000) {
      val cs = for (i <- 1 to 10) yield rnd.nextPrintableChar
      val res = Google.Query("foobar " + cs.mkString("")).execute()
      println(i + " = " + res.length)
    }
            
//    for (res <- Google.Query("Inception.1080p.BluRay.x264-REFiNED link:imdb.com/title/").execute()) {
//      println( res )
//    }

    ()
  }
}