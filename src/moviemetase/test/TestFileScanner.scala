package moviemetase
package test
import java.nio.file._

object TestFileScanner {

  def main(args: Array[String]): Unit = {
    
    val baseDir = Paths.get( args(0) )
    
    val q = FileScanner.findMovies(baseDir)
    
    val t = new Task[Unit] {
      def execute(): Unit = {
        println("TOOK " + q.take)
      }
    }
    t.execute()
  }
}