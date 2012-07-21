package moviemetase
package ui

import scala.swing.event.Event
import scala.swing.ScrollPane
import scala.swing.Component
import java.awt.Color
import javax.swing._
import javax.swing.event._
import javax.swing.border._
import comp._

case class SearchSelected(search: Search) extends Event

sealed trait SearchStatus
object SearchStatus {
  case object Pending   extends SearchStatus
  case object Searching extends SearchStatus
  case object Completed extends SearchStatus
  case object Failed    extends SearchStatus
}

case class Search(fileInfo: FileInfo, status: SearchStatus, result: List[Movie] = Nil, error: Option[Throwable] = None) {
//  override def equals(other: Any): Boolean = other match {
//    case Search(otherFileInfo, _, _) => fileInfo == otherFileInfo
//    case _ => false
//  }
}

class SearchesPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
  import SearchStatus._
  
  val cols =
    new TableCol("Status", 35) { maxWidth = prefWidth } ::
    new TableCol("Count", 20)  { maxWidth = prefWidth } ::
    TableCol("Dir/File", 200)  ::
    new TableCol("Open", 40)   { maxWidth = prefWidth; editable = true } ::
    Nil

  val model = new TableModel[Search](cols) {
    def getValue(search: Search, col: Int): AnyRef = col match {

      case 0 => (search.status, search.result.length, search.error) match {
        case (Pending,   _, _) =>
          ("/res/img/internet-bw.png", "queued for execution") 
        
        case (Searching, _, _) =>
          ("/res/img/internet.png", "searching ...")
        
        case (Completed, 0, _) =>
          ("/res/img/minus.png", "completed with no results")
        
        case (Completed, 1, _) =>
          ("/res/img/ok.png", "completed with one result, yay!")

        case (Completed, _, _) =>
          ("/res/img/plus.png", "completed with multiple results")

        case (Failed, _, Some(t)) =>
          ("/res/img/fail.png", "failed: " + t.getMessage)

        case x =>
          throw new Exception("invalid search-state: " + x) 
      }
      case 1 =>
        search.status match {
          case Completed => search.result.length.toString
          case _         => ""
        }
      case 2 => {
        val sb = new StringBuilder
        sb append "<html><pre style=\"font-face:sans-serif; font-size:10pt\">"
        sb append search.fileInfo.dirName
        sb append "\n/ "
        sb append search.fileInfo.fileName
        sb append "</pre></html>"
        sb.toString
        //(search.fileInfo.dirName, search.fileInfo.fileName)
      }
      case 3 =>
        new ImageIcon(App.resource("/res/img/open-dir.png"))
    }
  }
  
  val table = new JTable(model)
  table setTableHeader null
  table setSelectionMode ListSelectionModel.SINGLE_SELECTION

  cols.zipWithIndex.foreach({ case (col, idx) =>
    val colModel = table.getColumnModel.getColumn(idx) 
    colModel setPreferredWidth col.prefWidth
    if (col.maxWidth > 0)
      colModel setMaxWidth col.maxWidth
  })
  table setRowHeight 35

  table.getColumnModel.getColumn(0).setCellRenderer(TableLabelRenderer[(String, String)]({ case (lbl, value, row, col, selected) =>
    val (imgPath, text) = value
    
    lbl setIcon new ImageIcon(App.resource( imgPath ))
    lbl setToolTipText text
    
//    lbl setOpaque true
    
    if (selected) {
      lbl setForeground table.getSelectionForeground
      lbl setBackground table.getSelectionBackground
    } else {
      lbl setForeground table.getForeground
      lbl setBackground table.getBackground
    }

    /*
    lbl setForeground {
      if (selected) table.getSelectionForeground
      else table.getBackground
    }
    lbl setBackground  {
      if (selected) table.getSelectionBackground
      else table.getForeground
    }
    */
  }))

  val openDirAction = Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val search = model.rows( rowIdx )
    val f = new java.io.File( search.fileInfo.dirPath )
    UI.openFile(f)
  })
  new ButtonColumn(table, openDirAction, 3)
  
  contents = Component wrap table  
    
  table.getSelectionModel.addListSelectionListener(OnListSelectedEvent(evt => {
    val rowIdx = evt.getFirstIndex
    val selSearch = model.rows( rowIdx )
    SearchesPanel.this.publish( SearchSelected(selSearch) )
  }))
  
  def updateEq(a: Search, b:Search): Boolean = a.fileInfo == b.fileInfo
  def updateModel = model.update(updateEq) _

  listenTo( top.dropPanel )
  reactions += {
    case FoundMovieFile(fileInfo) => {
      updateModel( Search(fileInfo, SearchStatus.Pending) )
    }
    case SearchingMoviesByFile(fileInfo) => {
      updateModel( Search(fileInfo, SearchStatus.Searching) )
    }
    case FoundMoviesByFile(fileInfo, movies) => {
      updateModel( Search(fileInfo, SearchStatus.Completed, movies) )
    }
    case SearchingMoviesByFileFailed(fileInfo, t) => {
      updateModel( Search(fileInfo, SearchStatus.Failed, Nil, Some(t)) )
    }
  }
}