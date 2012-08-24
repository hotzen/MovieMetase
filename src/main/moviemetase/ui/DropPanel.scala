package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.nio.file._
import java.awt.{Component => JComponent}
import javax.swing.Icon
import moviemetase.search.MovieSearch
import comp._
import java.util.concurrent.BlockingQueue

case class FoundMovieFile(file: FileInfo) extends Event
case class SearchingMoviesByFile(file: FileInfo) extends Event
case class FoundMoviesByFile(file: FileInfo, movies: List[Movie]) extends Event
case class SearchingMoviesByFileFailed(file: FileInfo, t: Throwable) extends Event

class DropPanel(val top: UI) extends Component {

  override lazy val peer = new JImage( App.resource("/img/drop.png"), JImage.OriginalWidth, JImage.Blocking, JImage.NoCaching )
  
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
    val JComp: JComponent = null
    val Title = "Dropped Files"
    val Icon: Icon = null
    
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
    
    val scanDirsAnswer =
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
    
      
    val scanFiles = fs // scan all files
    
    val scanDirs = ds.flatMap(dir => scanDirsAnswer match {
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
    })
    
    val scanFilePaths = scanFiles.map( _.toPath )
    val scanDirPaths = scanDirs.map( _.toPath )
    
    def checkPath(p: Path): Boolean = {
      val fileInfo = FileInfo(p)
      val disFileInfo = Analyzer.dissectFileInfo( fileInfo )
      
      val tokens = disFileInfo.all.tokens
      if (tokens.find( Config.scanExcludes.contains(_) ).isDefined)
        return false
            
      if (!Analyzer.isExt( fileInfo.fileExt ))
        return false
      
      UI.publish(DropPanel.this)( FoundMovieFile(FileInfo(p)) )
      true
    }
    
    val q = FileScanner.findFiles(scanDirPaths, checkPath _, scanFilePaths)
    
    val search = new SearchTask(q) {
      def publish = UI.publish(DropPanel.this) _
      
      def onSearching(fileInfo: FileInfo) {
        publish( SearchingMoviesByFile(fileInfo) )
      }
      
      def onCompleted(fileInfo: FileInfo, movies: List[Movie]) {
        publish( FoundMoviesByFile(fileInfo, movies) )
      }
      
      def onFailed(fileInfo: FileInfo, t: Throwable) {
        publish( SearchingMoviesByFileFailed(fileInfo, t) )
      }
      
    }
    search.submit()
    
    ()
  }
}

abstract class SearchTask(q: BlockingQueue[Path]) extends Task[Unit] with DedicatedPool with Logging {
  val logID = "SearchTask"
  
  val pool = ("S", 2, 4)
  
  val searcher = new MovieSearch()

  def onSearching(fileInfo: FileInfo): Unit
  def onCompleted(fileInfo: FileInfo, movies: List[Movie]): Unit
  def onFailed(fileInfo: FileInfo, t: Throwable): Unit
    
  final def execute() {
    while (true) {
      val file = q.take() // blocking
      
      if (FileScanner.isTerminal(file))
        return ()
            
      val fileInfo = FileInfo(file)
      try {
        trace("searching by file '" + fileInfo + "'")
        onSearching( fileInfo )

        val movies = searcher searchByFile fileInfo // blocking
        
        info("found movies for " + fileInfo + ": " + movies.mkString(", "))
        onCompleted(fileInfo, movies)
        
      } catch { case t:Throwable =>
        error(t, "search for " + fileInfo + " failed")
        onFailed(fileInfo, t)
      }
    }
  }
}