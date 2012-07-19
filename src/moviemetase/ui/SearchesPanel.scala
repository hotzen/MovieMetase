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

case class Search(fileInfo: FileInfo, completed: Boolean, result: List[Movie])

class SearchesPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
  
  val cols =
    new TableCol("Status", 20) { maxWidth = prefWidth } ::
    new TableCol("Count", 20)  { maxWidth = prefWidth } ::
    TableCol("Dir/File", 150)  ::
    new TableCol("Open", 40)   { maxWidth = prefWidth; editable = true } ::
    Nil

  val model = new TableModel[Search](cols) {
    def getValue(search: Search, col: Int): AnyRef = col match {
      
      case 0 => (search.completed, search.result.length) match {
        case (true, 0)  => new ImageIcon(App.resource("/res/fail.png"))
        case (true, 1)  => new ImageIcon(App.resource("/res/ok.png"))
        case (true, _)  => new ImageIcon(App.resource("/res/plus.png"))
        case (false, _) => new ImageIcon(App.resource("/res/clouds.png"))
      }
      case 1 =>
        if (search.completed) search.result.length.toString
        else                  ""
      case 2 => {
        val sb = new StringBuilder
        sb append "<html>"
        sb append search.fileInfo.dirName
        sb append "<br>"
        sb append "<strong>/</strong>"
        sb append search.fileInfo.fileName
        sb append "</html>"
        sb.toString
      }
      case 3 =>
        new ImageIcon(App.resource("/res/open-dir.png"))
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
  table setRowHeight 30

  table.getColumnModel.getColumn(0).setCellRenderer(TableLabelRenderer[ImageIcon]({ case (lbl, value, row, col, selected) =>
    lbl setIcon value
  }))

  val openDirAction = Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val search = model.rows( rowIdx )
    val f = new java.io.File( search.fileInfo.dirPath )
    UI.desktop.foreach( dsk => dsk open f)
  })
  new ButtonColumn(table, openDirAction, 3)
  
  contents = Component wrap table  
    
  table.getSelectionModel.addListSelectionListener(OnListSelectedEvent(evt => {
    val rowIdx = evt.getFirstIndex
    val selSearch = model.rows( rowIdx )
    SearchesPanel.this.publish( SearchSelected(selSearch) )
  }))
  
  listenTo( top.dropPanel )
  reactions += {
    case SearchingMoviesByFile(fileInfo) => {
      model add Search(fileInfo, false, Nil)
    }
    case FoundMoviesByFile(fileInfo, movies) => {
      model remove (checkSearch => checkSearch.fileInfo == fileInfo)
      model add Search(fileInfo, true, movies)
    }
  }
}