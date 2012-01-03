package moviemetase
package test

import query._

object SubtitleSourceTest {
  def main(args: Array[String]) = {
    
    val s   = SubtitleSource.ReleaseSearch("Inception.1080p.BluRay.x264-REFiNED")
//    val fut = s.execute( "__Inception.:/1080p.BluRay.x264-REFiNED??" )
    val res = s.execute()
        
    println( res.mkString("\n") )
    ()
  }
}