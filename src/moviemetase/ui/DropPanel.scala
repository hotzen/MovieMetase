package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import moviemetase.search.MovieSearch
import java.awt.EventQueue

class DropPanel extends Label {

  override lazy val peer: javax.swing.JLabel = new JImageLabel( App.image("/res/drop.png") )
    
  tooltip = "DROP FILE HERE"
    
  val dropHandler = new FileDropHandler
  listenTo(dropHandler)
  peer.setTransferHandler(dropHandler)
      
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

    val q = FileScanner.createUnboundedQueue[Path]()
    
    // create a ScanTask for each dir
    val actualScanTasks = ds.flatMap(dir => dirsOption match {
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
    }).map(dir => FileScanner.findFilesTask(dir.toPath, q))
    
    // add a QueueTerminator
    val T = Paths.get("/TERMINATED/")
    val scanTasks = actualScanTasks ::: List( QueueTerminatorTask(q, T) )
        
    // search every file coming into the Q
    val safePublish = Events.publishEDT(this) _
    
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
            val movies = search searchByFile info
            
            println("RESULTS: {\n" + movies.mkString("\n") + "\n}\n")
            safePublish( Events.SearchResult(movies) )
          }
        }
      }
    }
    searchTask.submit()
        
    // execute the ScanTasks in sequence
    Task.createSeq( scanTasks ).submit()
    
    
        
//        None
//        showConfirmation (parent: Component = null, message: Any, title: String = uiString("OptionPane.titleText"), optionType: Value = Options.YesNo, messageType: Value = Message.Question, icon: Icon = EmptyIcon): Value
        
//        JOptionPane.showConfirmDialog(
//          comp,
//          sb.toString,
//          title,
//          JOptionPane.YES_NO_OPTION,
//          JOptionPane.QUESTION_MESSAGE
//        ) match {
//          case 0 => Some(dir)
//          case _ => None
//        }
//      }
//    })
    
//    val capacity = 10
//    val q = 
    
//    val searcher = new MovieSearch
//    val searchTask = new Task[List[Movie]] {
//      def execute(): List[Movie] = {
//        var ok = true
//        while (ok) {
//          val p = q.take()
//          if (p == FileScanner.TerminationObject) {
//            // done
//          } else {
//            // work
//          }
//        }
//        Nil
//      }
//    }
//    searchTask.submit()
//    
//    for (scanDir <- scanDirs) {
//      val s = FileScanner.findFiles(scanDir.toPath, q)
//      s.submit()
//      
//    }
    
    
    
    
    
//    
//    val fileQueue = new java.util.concurrent.ConcurrentLinkedQueue[java.io.File]
//    for (f <- fs) fileQueue offer f
//    
//    
//    
//    val scanner = new Task[List[java.io.File]] {
//      def execute(): List[java.io.File] = {
//        Nil
//      }
//    }
//    
//    val scanQs = scanDirs.flatMap(dir => {
//      val q = FileScanner.findMovies( dir.toPath )
//      
//      
//      None
//    }) 
    
      ()
  }
}