package moviemetase
package test

import query._
import search._

object GoogleTest {
  def main(args: Array[String]) = {
    
    { // CSE IMDB Search
      val title = "Terminator"
      val IMDB = "011282045967305256347:dyc6spozqnc"
      val q = GoogleCSE.Query(IMDB, title)
      val s = q.task()
      println ( s.call() )
    }
    
    { // Ajax Search
      val title = "Inception.1080p.BluRay.x264-REFiNED"
      val q = GoogleAjax.Query(title + " link:imdb.com/title/")
      val s = q.task()
      println ( s.call() )
    }

    ()
  }
}