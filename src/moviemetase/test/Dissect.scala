package moviemetase
package test
import java.io.File

object Dissect {
  def main(args: Array[String]) = {
    val f = new File( args(0) )
    
    // every line is a path
    val ps = scala.io.Source.fromFile(f, "utf-8").getLines.toList
    
    for (p <- ps) {
      val i = FileInfo(p)
      val d = DissectedFileInfo(i)
      
      println(p + "\n" + i + "\n" + d + "\n")
    }
  }
}