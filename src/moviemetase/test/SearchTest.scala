package moviemetase
package test

object SearchTest {
  def main(args: Array[String]) = {
    
    val ms  = new MovieSearch
    val res = ms.searchTerm( "Inception.1080p.BluRay.x264-REFiNED" )
    println( res.mkString("\n") )
    ()
  }
}