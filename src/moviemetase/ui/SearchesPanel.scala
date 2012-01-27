package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.swing.Icon

case class SearchRow(searching: Boolean, term: String, dir: String, file: String, path: String, movies: List[Movie])

case class SearchSelected(row: SearchRow) extends Event

class SearchesPanel(val top: UI) extends ScrollPane {
  //border = createBorder("Searches")
    
  val cols = 
    SmartTableCol(" ", 40) ::
    SmartTableCol("Movies found", 50)   ::
    SmartTableCol("Term", 100)          ::
    SmartTableCol("Directory", 300)     ::
    SmartTableCol("File", 300)          ::
    SmartTableCol("Full Path", 100)     ::
    Nil

  def getStatusIcon(row: SearchRow): ImageIcon = {
    val icon = 
      if (row.searching) "clouds.png"
      else if (row.movies.isEmpty) "fail.png"
      else if (!row.movies.isEmpty && row.movies.tail.isEmpty) "ok.png"
      else "plus.png"

    new ImageIcon( App.resource("/res/" + icon) )
  }
    
  val mdl = new SmartTableModel[SearchRow](cols)
  
  val tbl = new Table {
    model    = mdl 
    showGrid = true
    selection.intervalMode = Table.IntervalMode.Single
    
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
      val r = peer.convertRowIndexToModel(row)
      val c = peer.convertColumnIndexToModel(col)
      val o = mdl.rows(r)
      
      val NoIcon: Icon   = null
      val NoText: String = null
      
      val rdr = col match {
        case 0 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.icon = getStatusIcon(row) )
        case 1 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.text = row.movies.length.toString )
        case 2 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.text = row.term )
        case 3 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.text = row.dir )
        case 4 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.text = row.file )
        case 5 => new LabelRenderer[SearchRow]( (lbl,row) => lbl.text = row.path )
      }
      rdr.componentFor(this, sel, foc, o, row, col)
    }
  }
  
  val colMdl = tbl.peer.getColumnModel 
  for ( (col,idx) <- cols.zipWithIndex) {
    colMdl.getColumn(idx).setPreferredWidth( col.width )
  }
  
  contents = tbl
//  mdl.setPrefSize(tbl, 100)
  
  listenTo( tbl.selection )
  listenTo( top.dropPanel )
  
  reactions += {
    case SearchingMoviesByFile(file) => {
      mdl.remove(_.path == file.path)
      mdl add SearchRow(true, "", file.dirName, file.fileName, file.path, Nil)
    }

    case FoundMoviesByFile(file, movies) => {
      mdl remove (_.path == file.path)
      mdl add SearchRow(false, "", file.dirName, file.fileName, file.path, movies)
    }
  
    case TableRowsSelected(src, rng, false) => {
      for (rowIdx <- src.selection.rows) {
        val row = mdl.rows(rowIdx)
        publish( SearchSelected(row) )
      }
    }
  }
}