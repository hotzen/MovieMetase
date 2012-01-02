package moviemetase
package test

import query._
import search._

object GoogleTest {
  def main(args: Array[String]) = {
    
    { // CSE IMDB Search
      val title = "Terminator"
      val IMDB = "011282045967305256347:dyc6spozqnc"
      println( GoogleCSE.Query(IMDB, title).execute() )
    }
    
    { // Ajax Search
      val title = "Inception.1080p.BluRay.x264-REFiNED"
      println( GoogleAjax.Query(title + " link:imdb.com/title/").execute() )
    }

    ()
  }
}