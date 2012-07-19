package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.nio.file._
import moviemetase.search.MovieSearch
import comp._

case class SearchingMoviesByFile(file: FileInfo) extends Event
case class FoundMoviesByFile(file: FileInfo, movies: List[Movie]) extends Event

class DropPanel(val top: UI) extends Component {

  override lazy val peer = new JImage( App.resource("/res/drop.png"), JImage.OriginalWidth, JImage.Blocking, JImage.NoCaching )
  
  tooltip = "Drop directories or files or a mix of both here"
    
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
    
    val droppedDirsCount = ds.length
    val droppedDirs      = ( droppedDirsCount > 0)
    val droppedOneDir    = ( droppedDirsCount == 1)
    val droppedMultiDirs = ( droppedDirsCount > 1)
        
    val ScanDirAll  = "Scan all"
    val ScanDirAsk  = "Ask separately"
    val ScanDirDont = "Scan None"
    val ScanOpts = Array[Object](ScanDirAll, ScanDirAsk, ScanDirDont)
    
    val dirsOption =
      if (droppedOneDir) ScanDirAll
      
      else if (droppedMultiDirs) {
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
      } else ScanDirDont
    
    def checkPath(p: Path): Boolean =
      Analyzer.isExt( FileInfo(p).fileExt ) // only if registered extension

    val q = FileScanner.createQueue[Path]()
    
    // create a task that pushes all files into the Q
    val filesTask = new Task[Unit] {
      def execute(): Unit = {
        fs.filter(f => checkPath(f.toPath) ).foreach(f => q offer f.toPath)
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
    }).map(dir => FileScanner.findFilesTask(dir.toPath, checkPath, q))
    
    // Queue-Terminator
    val T = Paths.get("/TERMINATED/")
        
    val scannerTask = new Task[List[Unit]] {
      val terminatorTask = FileScanner.createTerminatorTask(q, T)
      val tasks: List[Task[Unit]] = filesTask :: dirScanTasks ::: terminatorTask :: Nil
      
      def execute(): List[Unit] = tasks.map( _.execute() ) // process in sequence
    }

    val publish = UI.publish(this) _
    
    val searchTask = new Task[Unit] with Logging {
      val logID = "SearchTask"
      val search = new MovieSearch()
  
      def execute(): Unit = {
        while (true) {
          val file = q.take() // blocking
          if (file == T)
            return ()
          
          val fileInfo = FileInfo(file)
          
          info("searching by file '" + fileInfo + "'")
          publish( SearchingMoviesByFile(fileInfo) )
          
          val movies = search searchByFile fileInfo // blocking
          
          info("search finished for file " + fileInfo)
          publish( FoundMoviesByFile(fileInfo, movies) )
        }
      }
    }
    
    searchTask.submit()
    scannerTask.submit()
    ()
  }
}