package moviemetase
package test

object TestDistinctWithCount {

  def main(args: Array[String]): Unit = {
    import Util._
    
    val xs = "a b b c a b".split(" ").toList
    val ys = xs.distinctWithCount()
    
    println( ys.mkString("\n") )
  }

}