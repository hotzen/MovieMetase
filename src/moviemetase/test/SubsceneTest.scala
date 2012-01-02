package moviemetase
package test

import query._

object SubsceneTest {
  def main(args: Array[String]) = {
    
    val q1 = Subscene.SearchByRelease("Inception.1080p.BluRay.x264-REFiNED")
    println( q1.execute() )
    
    println(" ======================= ")
    
    val q2 = Subscene.SearchByTitle("Inception")
    println( q2.execute() )
    
    ()
  }
}