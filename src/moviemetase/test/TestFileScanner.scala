package moviemetase
package test
import java.nio.file._

object TestFileScanner {

  def main(args: Array[String]): Unit = {
    
    val baseDir = Paths.get( args(0) )
    
    val q = FileScanner.createQueue[Path](100)
    val t = FileScanner.findFilesTask(baseDir, (f => true), q)
    
    val t2 = new Task[Unit] {
      def execute(): Unit = {
        while (true) {
          println("TOOK " + q.take)
        }
      }
    }
    t.submit()
    t2.execute()
  }
}