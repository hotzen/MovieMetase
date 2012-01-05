package moviemetase
package test

import sites._
import search._

object OpenSubtitlesLoginTest {
  def main(args: Array[String]) = {
    
    val q = OpenSubtitles.Login()
    println( q.execute() )
    
    ()
  }
}