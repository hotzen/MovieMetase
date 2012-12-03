package moviemetase
package ui

import comp._
import scala.swing._
import scala.swing.event._
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.ImageIcon

class SubtitlesPanel(val top: UI) extends ScrollPane {
  
  val cols =
    new CheckboxCol("Select", 80)                                          :: // 0
    new TableCol("Label",    200)                                          :: // 1
    new TableCol("Release",  200)                                          :: // 2
    new TableCol("Language", 200)                                          :: // 3
    new TableCol("Visit",    80) { maxWidth = prefWidth; editable = true } :: // 4
    new TableCol("Download", 80) { maxWidth = prefWidth; editable = true } :: // 5
    new TableCol("Website",  200)                                          :: // 6
    Nil

  val model = new TableModel[MovieInfos.Subtitle](cols) {
    def getValue(sub: MovieInfos.Subtitle, col: Int): AnyRef = col match {
      case 0 => Boolean.box(false) // TODO
      case 1 => sub.label
      case 2 => sub.releaseText.getOrElse("-")
      case 3 => sub.langText
      case 4 => new ImageIcon(App.resource("/img/visit.png")) //sub.website.toExternalForm
      case 5 => sub.download match {
        case Some(url) => new ImageIcon(App.resource("/img/download.png"))
        case None      => "-"
      }
      case 6 => sub.website.toExternalForm
    }
  }
  
  val table = new JTable(model)
  //table setTableHeader null
  //table setSelectionMode ListSelectionModel.SINGLE_INTERVAL_SELECTION //SINGLE_SELECTION
  
  cols.zipWithIndex.foreach({ case (col, idx) =>
    val colModel = table.getColumnModel.getColumn(idx) 
    colModel setPreferredWidth col.prefWidth
    if (col.maxWidth > 0)
      colModel setMaxWidth col.maxWidth
  })
  table setRowHeight 35
  
  val visitAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val subtitle = model.rows( rowIdx )
    UI.openBrowser( subtitle.website.toURI )
  })
  new ButtonColumn(table, visitAction, 3)
    
  val downloadAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val subtitle = model.rows( rowIdx )
    for (dl <- subtitle.download) {
      //TODO
      //dispatch for download subtitle.download
    }
  })
  new ButtonColumn(table, downloadAction, 4)
    
  contents = Component wrap table  
  
//  table.getSelectionModel.addListSelectionListener(OnListSelectedIndex(selIdx => {
//    val sel = model.rows( selIdx )
//  }))
  
  def updateEq(a: MovieInfos.Subtitle, b:MovieInfos.Subtitle): Boolean = a.website == b.website
  def updateModel = model.update(updateEq) _

  listenTo( top )
  reactions += {
    case MovieSelected(m) => {
      model.clear()
      for (sub <- m.infos.collect({ case i:MovieInfos.Subtitle => i}))
        model add sub
    }
  }
}
