package moviemetase
package test

object GoogleTest {
  def main(args: Array[String]) = {
    
    println("hello world")
    
    //val title = "Little.Fockers.720p.BluRay.x264-TWiZTED"
    val title = "Terminator"
    
    val imdb = GoogleQuery.IMDB(title)
   // val subs = GoogleQuery.Subtitles(title)
    
    val s1 = GoogleSearch(imdb)
    //val s2 = GoogleSearch(subs)
    
    val r = s1.call()
    //s2.call()
    
    println( r.mkString("\n\n") )
    
    ()
  }
}