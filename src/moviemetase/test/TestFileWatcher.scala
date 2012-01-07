package moviemetase
package test
import java.nio.file._

object TestFileWatcher {

  def main(args: Array[String]): Unit = {
    
    val baseDir = Paths.get( args(0) )
    
    val q = FileWatcher.watch(baseDir)
    
    val t = new Task[Unit] {
      def execute(): Unit = {
        while (true) {
          println( q.take() )
        }
      }
    }
    t.execute()
  }
}