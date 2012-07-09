package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue

object FileScanner {
  def createQueue[A](capacity: Int = Int.MaxValue): BlockingQueue[A] =
    new java.util.concurrent.LinkedBlockingQueue[A]( capacity )
  
//  def createUnboundedQueue[A](): BlockingQueue[A] = 
//    new java.util.concurrent.LinkedBlockingQueue[A]( )
    
  def findFilesTask(baseDir: Path, filter: Path => Boolean, queue: BlockingQueue[Path]): FileScannerTask[Path] =
    new FileScannerTask(baseDir, new FileCollector(queue, filter))
  
  def createTerminatorTask[A](queue: BlockingQueue[A], terminator: A): Task[Unit] = new Task[Unit] {
    def execute(): Unit = {
      queue offer terminator
      ()
    }
  }
}

trait QueueingFileVisitor[A] extends FileVisitor[Path] {
  val queue: BlockingQueue[A]
}

//case class QueueTerminatorTask[A](queue: BlockingQueue[A], terminator: A) extends Task[Unit] {
//  def execute(): Unit = {
//    queue offer terminator
//    ()
//  }
//}

case class FileScannerTask[A](baseDir: Path, visitor: QueueingFileVisitor[A]) extends Task[Unit] {
  def queue: BlockingQueue[A] = visitor.queue
    
  def execute(): Unit = {
    Files.walkFileTree(baseDir, visitor)
    ()
  }
}

class FileCollector(val queue: BlockingQueue[Path], val filter: Path => Boolean) extends QueueingFileVisitor[Path] {
  import java.nio.file.FileVisitResult._
  
  private var firstDir: Path = null
  
  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = CONTINUE
  def postVisitDirectory(path: Path, ex: IOException): FileVisitResult =  CONTINUE
  
  def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    if (filter(path))
      queue put path
    CONTINUE
  }
    
  def visitFileFailed(path: Path, ex: IOException): FileVisitResult = {
    // TODO
    CONTINUE
  }
}




//
//final class MovieCollector(val queue: BlockingQueue[FileInfo]) extends QueueingFileVisitor[FileInfo] with Logging {
//  import java.nio.file.FileVisitResult._
//  
//  val logID = "MovieCollector"
//    
////  val dirs: Stack[String] = Stack()
//  
//  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = CONTINUE 
////  {
////    val dirName = path.getName(path.getNameCount - 1).toString
////    dirs push dirName
////    CONTINUE
////  }
//  
//  def postVisitDirectory(path: Path, ex: IOException): FileVisitResult = CONTINUE
////  {
////    dirs.pop
////    CONTINUE
////  }
//  
//  def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
//    val info = FileInfo(path)
//    
//    if (Analyzer isExt info.fileExt) {
//      trace("[Q="+("%03d" format queue.size)+"] " + info)
//      queue put info // blocking
//    } else {
//      trace("\tSKIP " + info)
//    }
//    
//    //trace("\t path-stack: " + dirs.reverse.mkString(" / "))
//    CONTINUE
//  }
//  
//  def visitFileFailed(path: Path, ex: IOException): FileVisitResult = {
//    warn("invalid file " + path.toAbsolutePath() + ": " + ex.getMessage())
//    CONTINUE
//  }
//}