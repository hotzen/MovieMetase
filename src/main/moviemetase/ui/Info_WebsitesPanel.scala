package moviemetase
package ui

import comp._
import scala.swing._
import scala.swing.event._
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.ImageIcon

class WebsitesPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
    
  val cols =
    new CheckboxCol("Select", 80)                                       :: // 0
    new TableCol("Type", 80)                                            :: // 1
    new TableCol("Name", 300)                                           :: // 2
    new TableCol("Visit", 80) { maxWidth = prefWidth; editable = true } :: // 3
    new TableCol("URL",  200)                                           :: // 4
    Nil

  val model = new TableModel[MovieInfos.Website](cols) {
    def getValue(info: MovieInfos.Website, col: Int): AnyRef = col match {
      case 0 => Boolean.box(false) // TODO
      case 1 => info match {
        case i:MovieInfos.Trailer  => "Trailer"
        case i:MovieInfos.Subtitle => "Subtitle"
        case _ => "Website"
      }
      case 2 => info match {
        case i:MovieInfos.Trailer  => i.label
        case i:MovieInfos.Subtitle => i.label
        case i => i.websiteTitle
      }
      case 3 => info.website.toExternalForm
      case 4 => new ImageIcon(App.resource("/img/visit.png"))
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

  val visitAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val info = model.rows( rowIdx )
    UI.openBrowser( info.website.toURI )
  })
  new ButtonColumn(table, visitAction, 3)
  
  contents = Component wrap table  
    
//  table.getSelectionModel.addListSelectionListener(OnListSelectedIndex(selIdx => {
//    val sel = model.rows( selIdx )
//  }))
  
  def updateEq(a: MovieInfo, b: MovieInfo): Boolean = a == b
  def updateModel = model.update(updateEq) _

  listenTo( top )
  reactions += {
    case MovieSelected(m) => {
      model.clear()
      val infos = m.infos.flatMap(_ match {
        case i:MovieInfos.Subtitle => Some(i)
        case i:MovieInfos.Trailer  => Some(i)
        case i:MovieInfos.Website  => Some(i)
        case _ => None
      })
      for (info <- infos)
        model add info
    }
  }
}