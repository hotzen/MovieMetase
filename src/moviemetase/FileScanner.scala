package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.LinkedBlockingQueue

object FileScanner {
  
  val T = Paths.get("/TERMINATED/")
  
  def findFiles(dirs: List[Path], p: Path => Boolean, terminate: Boolean = true, pauseFindCount: Int = Int.MaxValue): BlockingQueue[Path] = {
    val q = new LinkedBlockingQueue[Path]( pauseFindCount )
    val v = new FileCollector(q, p)
    
    val optT =
      if (terminate) Some(T)
      else None
    
    new FileScannerTask(dirs, v, optT).submit() // async
    q
  }
}

trait QueueingFileVisitor extends FileVisitor[Path] {
  def q: BlockingQueue[Path]
}

class FileScannerTask[A](dirs: List[Path], v: QueueingFileVisitor, T: Option[Path]) extends Task[Unit] {
  def execute() {
    
    for (dir <- dirs)
      Files.walkFileTree(dir, v)

    if (T.isDefined)
      v.q put T.get // terminate
  }
}

class FileCollector(val q: BlockingQueue[Path], p: Path => Boolean) extends QueueingFileVisitor with Logging {
  import java.nio.file.FileVisitResult._
  val logID = "FileCollector"
    
  var dirs = List[Path]() 
    
  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    dirs = path :: dirs
    CONTINUE
  }
  
  def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    if (p(path))
      q put path // blocking

    CONTINUE
  }
  
  def visitFileFailed(path: Path, ex: IOException): FileVisitResult = {
    warn("failed to visit " + path.toAbsolutePath + ": " + ex.getMessage)
    SKIP_SUBTREE
  }
  
  def postVisitDirectory(path: Path, ex: IOException): FileVisitResult = {
    dirs = dirs.tail
    CONTINUE
  }
}