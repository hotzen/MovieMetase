package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import moviemetase.search.MovieSearch

case class SearchingMoviesByFile(file: FileInfo) extends Event
case class FoundMoviesByFile(file: FileInfo, movies: List[Movie]) extends Event

class DropPanel(val top: UI) extends Label {

  override lazy val peer: javax.swing.JLabel = new ImageLabel( App.resource("/res/drop.png"), ImageLabel.OriginalWidth, ImageLabel.Blocking )
    
  tooltip = "Drop directories, files or both here"
    
  val dropHandler = new FileDropHandler
  peer.setTransferHandler(dropHandler)
  listenTo(dropHandler)
      
  reactions += {
    case FileDropHandler.FilesDropped(files) => processDroppedFiles(files)
  }
  
  def processDroppedFiles(files: List[java.io.File]): Unit = {
    import Dialog._
    
    val Comp: Component = null
    val JComp: java.awt.Component = null
    val Title = "Dropped Files"
    val Icon: javax.swing.Icon = null
    
    if (files.isEmpty) {
      Dialog.showMessage(Comp, "No files or directories dropped", Title, Message.Error)
      return ()
    }
    
    var (ds, fs) = files.partition(_.isDirectory)
    
    val droppedDirsCount = files.count( _.isDirectory )
    val droppedDirs      = ( droppedDirsCount > 0)
    val droppedMultiDirs = ( droppedDirsCount > 1)
        
    val ScanDirAll  = "Scan all"
    val ScanDirAsk  = "Ask separately"
    val ScanDirDont = "Scan None"
    val ScanOpts = Array[Object](ScanDirAll, ScanDirAsk, ScanDirDont)
    
    val dirsOption =
      if (droppedMultiDirs) {
        val sb = new StringBuilder
        sb append "Scan the following directories for movies?\n"
        for (dir <- files if dir.isDirectory)
          sb append "  " append dir.getAbsolutePath append "\n"
                    
        javax.swing.JOptionPane.showOptionDialog(
          JComp,
          sb.toString,
          Title,
          0,
          javax.swing.JOptionPane.QUESTION_MESSAGE,
          Icon,
          Array[Object](ScanDirAll, ScanDirAsk, ScanDirDont),
          ScanDirAll
        ) match {
          case 0  => ScanDirAll
          case 1  => ScanDirAsk
          case _  => ScanDirDont
        }
      } else ScanDirAsk
    
    import java.nio.file._

    def filter(p: Path): Boolean =
      Analyzer.isExt( FileInfo(p).fileExt ) // only if registered extension
          
    val q = FileScanner.createUnboundedQueue[Path]()
    
    // create a task that pushes all files into the Q
    val filesTask = new Task[Unit] {
      def execute(): Unit = {
        fs.filter(f => filter(f.toPath) ).foreach(f => q offer f.toPath)
      }
    }

    // create a ScanTask for each dir
    val dirScanTasks = ds.flatMap(dir => dirsOption match {
      case ScanDirAll  => Some(dir)
      case ScanDirDont => None
      case ScanDirAsk  => {
        val sb = new StringBuilder
        sb append "Scan the following directory for movies?\n"
        sb append "  " append dir.getAbsolutePath
        
        Dialog.showConfirmation(
          Comp,
          sb.toString,
          Title
        ) match {
          case Dialog.Result.Ok => Some(dir)
          case Dialog.Result.No => None
        }
      }
    }).map(dir => FileScanner.findFilesTask(dir.toPath, filter, q))
    
    // add a QueueTerminator
    val T = Paths.get("/TERMINATED/")
    val collectTasks: List[Task[Unit]] = filesTask :: dirScanTasks ::: List( QueueTerminatorTask(q, T) )
    
    // search every file coming into the Q
    val safePublish = UI.publish(this) _
    
    val searchTask = new Task[Unit] {
      val search = new MovieSearch()
      def execute(): Unit = {
        var continue = true
        while (continue) {
          val file = q.take() // blocking
          if (file == T) {
            println("TERMINATOR")
            continue = false
          } else {
            val info = FileInfo(file)
            safePublish( SearchingMoviesByFile(info) )
            
            val movies = search searchByFile info
            safePublish( FoundMoviesByFile(info, movies) )
          }
        }
      }
    }
    
    searchTask.submit()
    Task.createSeq( collectTasks ).submit()
    ()
  }
}