package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue
import java.util.Queue

object FileScanner {
  var QueueCapacity: Int = 100
  
  def createQueue[A](capacity: Int = QueueCapacity): BlockingQueue[A] =
    new java.util.concurrent.LinkedBlockingQueue[A]( capacity ) // bounded
    
  def findMovies(baseDir: Path): BlockingQueue[FileInfo] = {
    val q = createQueue[FileInfo]()
    val v = new MovieCollector( q )
    val s = new FileScanner(baseDir, v)
    s.submit()
    q
  }
}

trait QueueingFileVisitor[A] extends FileVisitor[Path] {
  def queue: BlockingQueue[A]
}

final class FileScanner[A](baseDir: Path, visitor: QueueingFileVisitor[A]) extends Task[Unit] {
  val queue: BlockingQueue[A] = visitor.queue
  
  def execute(): Unit = {
    Files.walkFileTree(baseDir, visitor)
    ()
  }
}

object FileWatcher {
  def createQueue[A](): BlockingQueue[A] =
    new java.util.concurrent.LinkedBlockingQueue[A]() // unbounded
    
  def watch(p: Path): BlockingQueue[FileWatchEvent] = {
      import java.nio.file.StandardWatchEventKinds._
      
      val q = createQueue[FileWatchEvent]()
      val w = new FileWatcher(p, q)
      
      w.register( Array(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY) )
      w.submit()
      
      q
    }
}

sealed trait FileWatchEvent
  case object Overflow               extends FileWatchEvent 
  case class EntryCreate(path: Path) extends FileWatchEvent 
  case class EntryDelete(path: Path) extends FileWatchEvent 
  case class EntryModify(path: Path) extends FileWatchEvent 


final class FileWatcher(path: Path, queue: BlockingQueue[FileWatchEvent]) extends Task[Unit] {
  import scala.collection.JavaConversions._
  import java.nio.file.StandardWatchEventKinds._
  
  val service = FileSystems.getDefault().newWatchService()
  
  def register(interest: Array[WatchEvent.Kind[_]]): WatchKey =
    path.register(service, interest)
  
  def execute(): Unit = {
    var ok = true
    while (ok) {
      
      val key =
        try   { service.take() } // blocking
        catch { case e:InterruptedException => return }
      
      for (evt <- key.pollEvents())
        processEvent( evt.asInstanceOf[WatchEvent[Path]] )

      ok = key.reset()
    }
  }
  
  def processEvent(evt: WatchEvent[Path]): Unit = {
    val kind: WatchEvent.Kind[_] = evt.kind
    kind match {
      case OVERFLOW     => queue offer Overflow
      case ENTRY_CREATE => queue offer EntryCreate(evt.context)
      case ENTRY_DELETE => queue offer EntryDelete(evt.context)
      case ENTRY_MODIFY => queue offer EntryModify(evt.context)
      case _            => // ?
    }
  }
}



final class MovieCollector(val queue: BlockingQueue[FileInfo]) extends QueueingFileVisitor[FileInfo] with Logging {
  import java.nio.file.FileVisitResult._
  
  val logID = "MovieCollector"
    
  val dirs: Stack[String] = Stack()
  
  def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
    val dirName = path.getName(path.getNameCount - 1).toString
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