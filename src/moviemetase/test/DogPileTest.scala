package moviemetase
package test

import sites._

object DogPileTest {
  def main(args: Array[String]) {
    
    val res = DogPile.Query("Wolfman.UNRATED.1080p.BluRay.x264-REFiNED link:imdb.com/title/").execute()
    
    for (r <- res) {
      println(r)
    }
  }
}