package moviemetase
package test

import sites._

object GoogleWebTest {
  def main(args: Array[String]) {
    
    val res = GoogleWeb.Query("Wolfman.UNRATED.1080p.BluRay.x264-REFiNED link:imdb.com/title/").execute()
    
//    for (r <- res) {
//      println(res)
//    }
  }
}