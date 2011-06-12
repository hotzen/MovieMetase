package moviemetase
package test

object GoogleTest {
  def main(args: Array[String]) = {
    
    { // CSE IMDB Search
      val title = "Terminator"
      val IMDB = "011282045967305256347:dyc6spozqnc"
      val q = GoogleCSE.Query(IMDB, title)
      val s = GoogleSearch(q)
      println ( s.call() )
    }
    
    { // Ajax Search
      val title = "Inception.1080p.BluRay.x264-REFiNED"
      val q = GoogleAjax.Query(title + " link:imdb.com/title/")
      val s = GoogleSearch(q)
      println ( s.call() )
    }

    ()
  }
}