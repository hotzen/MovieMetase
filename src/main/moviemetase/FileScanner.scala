package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.LinkedBlockingQueue

object FileScanner {
  
  val T = Paths.get("/TERMINAL/")
  
  def isTerminal(p: Path): Boolean = p eq T
  
  def pushFiles(q: BlockingQueue[Path], fs: List[Path]) {
    new Task[Unit] {
      def execute(): Unit = fs.foreach(q put _)
    }.submit()
  }
  
  def findFiles(dirs: List[Path], p: Path => Boolean, forceFiles: List[Path] = Nil): BlockingQueue[Path] = {
    val q = new LinkedBlockingQueue[Path]( Int.MaxValue )
    forceFiles.foreach(q put _)

    val v = new FileCollector(q, p)
    new FileScannerTask(dirs, v, Some(T)).submit()
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
    
  var stack = List[Path]()

  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    stack = path :: stack
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
    stack = stack.tail
    CONTINUE
  }
}