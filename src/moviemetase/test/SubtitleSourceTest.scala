package moviemetase
package test

import search._

object SubtitleSourceTest {
  def main(args: Array[String]) = {
    
    val s   = SubtitleSource.ReleaseSearch("SubtitleSourceReleaseTest")
//    val fut = s.execute( "__Inception.:/1080p.BluRay.x264-REFiNED??" )
    val fut = s.execute( "Inception.1080p.BluRay.x264-REFiNED" )
    val res = fut.get()
    
    println( res.mkString("\n") )
    ()
  }
}