package moviemetase
package test

object GoogleTest {
  def main(args: Array[String]) = {
    
    { // CSE IMDB Search
      val title = "Terminator"
      val cse = "011282045967305256347:dyc6spozqnc"
      val q = GoogleCSE.Query(title, cse)
      val s = GoogleSearch(q)
//      val r = s.call()
//      println( r.mkString("\n\n") )
    }
    
    { // Raw Search "Inception.1080p.BluRay.x264-REFiNED" link:imdb.com/title/tt*
      val title = "Inception.1080p.BluRay.x264-REFiNED"
      val q = GoogleAjax.Query(title, "+link:imdb.com/title/")
      val s = GoogleSearch(q)
      
      println( q.url.toString )
      val r = s.call()
      
    }
    
    ()
  }
}