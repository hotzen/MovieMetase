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

sealed trait SearchStatus
object SearchStatus {
  case object Pending   extends SearchStatus
  case object Searching extends SearchStatus
  case object Completed extends SearchStatus
  case object Failed    extends SearchStatus
}

case class Search(fileInfo: FileInfo, status: SearchStatus, result: List[Movie] = Nil, error: Option[Throwable] = None)

class SearchesPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
  import SearchStatus._
  
  val cols =
    new TableCol("Status", 35) { maxWidth = prefWidth } ::
    new TableCol("Count", 20)  { maxWidth = prefWidth } ::
    new TableCol("Dir/File", 200)  ::
    new TableCol("Open", 40)   { maxWidth = prefWidth; editable = true } ::
    Nil

  val model = new TableModel[Search](cols) {
    def getValue(search: Search, col: Int): AnyRef = col match {

      case 0 => (search.status, search.result.length, search.error) match {
        case (Pending,   _, _) =>
          ("/img/internet-bw.png", "queued for execution") 
        
        case (Searching, _, _) =>
          ("/img/internet.png", "searching ...")
        
        case (Completed, 0, _) =>
          ("/img/minus.png", "completed with no results")
        
        case (Completed, 1, _) =>
          ("/img/ok.png", "completed with one result, yay!")

        case (Completed, _, _) =>
          ("/img/plus.png", "completed with multiple results...")

        case (Failed, _, Some(t)) =>
          ("/img/fail.png", "failed: " + t.getMessage)

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
      }
      case 3 =>
        new ImageIcon(App.resource("/img/open-dir.png"))
    }
  }
  
  val table = new JTable(model)
  table setTableHeader null
  table setSelectionMode ListSelectionModel.SINGLE_INTERVAL_SELECTION //SINGLE_SELECTION
  //table setRowSelectionAllowed true
  //table setColumnSelectionAllowed false
  //table setCellSelectionEnabled false
  
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

  val openDirAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val search = model.rows( rowIdx )
    val f = new java.io.File( search.fileInfo.dirPath )
    UI.openFile(f)
  })
  new ButtonColumn(table, openDirAction, 3)
  
  contents = Component wrap table  
    
  table.getSelectionModel.addListSelectionListener(OnListSelectedIndex(selIdx => {
    val selSearch = model.rows( selIdx )
    SearchesPanel.this.publish( SearchSelected(selSearch) )
  }))
  
  def updateEq(a: Search, b:Search): Boolean = a.fileInfo == b.fileInfo
  def updateModel = model.update(updateEq) _

  listenTo( top.dropPanel )
  reactions += {
    case MovieFileScanned(fileInfo) =>
      updateModel( Search(fileInfo, SearchStatus.Pending) )
    
    case SearchingMoviesByFile(fileInfo) =>
      updateModel( Search(fileInfo, SearchStatus.Searching) )
    
    case FoundMoviesByFile(fileInfo, movies) =>
      updateModel( Search(fileInfo, SearchStatus.Completed, movies) )
    
    case SearchingMoviesByFileFailed(fileInfo, t) =>
      updateModel( Search(fileInfo, SearchStatus.Failed, Nil, Some(t)) )
  }
}