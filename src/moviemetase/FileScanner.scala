package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue

object FileScanner {
  
  def createQueue[A](capacity: Int): BlockingQueue[A] =
    new java.util.concurrent.LinkedBlockingQueue[A]( capacity )
    
  def findMovies(baseDir: Path): BlockingQueue[FileInfo] = {
    val q = createQueue[FileInfo](100)
    val v = new MovieCollector( q )
    val s = new FileScanner(baseDir, v)
    s.submit()
    q
  }
}

trait QueueingFileVisitor[A] extends FileVisitor[Path] {
  val queue: BlockingQueue[A]
}

class FileScanner[A](baseDir: Path, visitor: QueueingFileVisitor[A]) extends Task[Unit] {
  val queue: BlockingQueue[A] = visitor.queue
  
  def execute(): Unit = {
    Files.walkFileTree(baseDir, visitor)
    ()
  }
}



class MovieCollector(val queue: BlockingQueue[FileInfo]) extends QueueingFileVisitor[FileInfo] with Logging {
  import java.nio.file.FileVisitResult._
  
  val logID = "MovieCollector"
    
  val dirs: Stack[String] = Stack()
  
  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    val dirName = path.getName(path.getNameCount() - 1).toString
    dirs push dirName
    CONTINUE
  }
  
  def postVisitDirectory(path: Path, ex: IOException): FileVisitResult = {
    dirs.pop
    CONTINUE
  }
  
  def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    val info = FileInfo(path)
        
    if (Analyzer isExt info.fileExt) {
      trace("[Q="+("%03d" format queue.size)+"] " + info)
      queue put info // blocking
    } else {
      trace("\tSKIP " + info)
    }
    
    //trace("\t path-stack: " + dirs.reverse.mkString(" / "))
    CONTINUE
  }
  
  def visitFileFailed(path: Path, ex: IOException): FileVisitResult = {
    warn("invalid file " + path.toAbsolutePath() + ": " + ex.getMessage())
    CONTINUE
  }
}