package moviemetase
package ui

import comp._
import scala.swing._
import scala.swing.event._
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.ImageIcon

class PostersPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
  
  import MovieInfos.Poster
  
  val cols =
    new CheckboxCol("Select", 80)                                        :: // 0
    new TableCol("Status", 80) { maxWidth = prefWidth }                  :: // 1
    new TableCol("Visit", 80)  { maxWidth = prefWidth; editable = true } :: // 2
    new TableCol("Image", 600)                                           :: // 3
    Nil

  val model = new TableModel[Poster](cols) {
    def getValue(poster: Poster, col: Int): AnyRef = col match {
      case 0 => Boolean.box(false)
      case 1 => ""
      case 2 => new ImageIcon(App.resource("/img/internet.png"))
      case 3 => poster.url.toString
    }
  }
  
  val table = new JTable(model)
  //table setTableHeader null
  //table setSelectionMode ListSelectionModel.SINGLE_INTERVAL_SELECTION //SINGLE_SELECTION
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

//  table.getColumnModel.getColumn(0).setCellRenderer(TableLabelRenderer[(String, String)]({ case (lbl, value, row, col, selected) =>
//    val (imgPath, text) = value
//      
//    if (selected) {
//      lbl setForeground table.getSelectionForeground
//      lbl setBackground table.getSelectionBackground
//    } else {
//      lbl setForeground table.getForeground
//      lbl setBackground table.getBackground
//    }
//  }))
  
  val visitAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val info = model.rows( rowIdx )
    UI.openBrowser( info.url.toURI )
  })
  new ButtonColumn(table, visitAction, 2)

  contents = Component wrap table  
    
  table.getSelectionModel.addListSelectionListener(OnListSelectedIndex(selIdx => {
    val selSearch = model.rows( selIdx )
    //InfoWebsitesPanel.this.publish( SearchSelected(selSearch) )
  }))
  
  def updateEq(a: MovieInfo, b: MovieInfo): Boolean = a == b
  def updateModel = model.update(updateEq) _

  listenTo( top )
  reactions += {
    case MovieSelected(movie) => {
      model.clear()
      val posters = movie.infos.collect({ case i:MovieInfos.Poster => i })
      for (poster <- posters)
        model add poster
    }
  }
}