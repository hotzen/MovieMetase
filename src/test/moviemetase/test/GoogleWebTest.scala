package moviemetase
package test

import sites._

object GoogleWebTest {
  def main(args: Array[String]) {
    
    //val res = GoogleWeb.Query("Wolfman.UNRATED.1080p.BluRay.x264-REFiNED link:imdb.com/title/").execute()
    val res = Google.Query("\"Inception.1080p.BluRay.x264-REFiNED\" link:imdb.com").execute()
    
    for (r <- res) {
      println(r)
    }
  }
}