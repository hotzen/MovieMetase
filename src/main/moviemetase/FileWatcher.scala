package moviemetase

import java.nio.file._
import java.nio.file.attribute._
import java.io.IOException
import scala.collection.mutable.Stack
import java.util.concurrent.BlockingQueue

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
object FileWatchEvent {
  case object Overflow           extends FileWatchEvent 
  case class Create(path: Path)  extends FileWatchEvent 
  case class Delete(path: Path)  extends FileWatchEvent 
  case class Modify(path: Path)  extends FileWatchEvent 
}
  
final class FileWatcher(path: Path, queue: BlockingQueue[FileWatchEvent]) extends Task[Unit] {
  import scala.collection.JavaConversions._
  import java.nio.file.StandardWatchEventKinds._
  
  val service = FileSystems.getDefault.newWatchService
  
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
  
  private def processEvent(evt: WatchEvent[Path]): Unit = {
    val kind: WatchEvent.Kind[_] = evt.kind
    kind match {
      case OVERFLOW     => queue offer FileWatchEvent.Overflow 
      case ENTRY_CREATE => queue offer FileWatchEvent.Create(evt.context)
      case ENTRY_DELETE => queue offer FileWatchEvent.Delete(evt.context)
      case ENTRY_MODIFY => queue offer FileWatchEvent.Modify(evt.context)
    }
  }
}
