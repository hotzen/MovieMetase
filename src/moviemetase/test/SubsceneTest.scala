package moviemetase
package test

import search._

object SubsceneTest {
  def main(args: Array[String]) = {
    
    val q = Subscene.Release("Inception")
    val t = q.task()
    println( t.call() )
    
    ()
  }
}