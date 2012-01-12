package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import javax.swing.ImageIcon
import scala.swing.Table.LabelRenderer
import javax.swing.Icon

case class SearchRow(searching: Boolean, term: String, dir: String, file: String, path: String, movies: List[Movie])

class SearchStatusRenderer extends Label with TableRendererComp[SearchRow] {
  def render(o: SearchRow, sel: Boolean, foc: Boolean): Unit = {
    val url =
      if (o.searching)
        App.resource("/res/spinner.gif")
      else if (o.movies.isEmpty)
        App.resource("/res/face-surprise.gif")
      else if (!o.movies.isEmpty && o.movies.tail.isEmpty)
        App.resource("/res/face-grin.gif")
      else
        App.resource("/res/face-plain.gif")
    
    println("SearchStatusRenderer " + o + " " + url)
    icon = new ImageIcon(url)
  }
}

case class SearchRowSelected(row: SearchRow) extends Event

class SearchPanel(val top: UI) extends ScrollPane {
  //border = createBorder("Searches")
    
  val cols = 
    SmartTableCol("Searching", 10) ::
    SmartTableCol("Movies found", 50)   ::
    SmartTableCol("Term", 100)          ::
    SmartTableCol("Directory", 300)     ::
    SmartTableCol("File", 300)          ::
    SmartTableCol("Full Path", 100)     ::
    Nil

  val mdl = new SmartTableModel[SearchRow](cols)
    
  val tbl = new Table {
    model    = mdl 
    showGrid = true
    selection.intervalMode = Table.IntervalMode.Single
    
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
      val r = peer.convertRowIndexToModel(row)
      val c = peer.convertColumnIndexToModel(col)
      val o = mdl.rows(r)
      
      val NoIcon: Icon = null
      
      val rdr = row match {
        case 0 => new TableRenderer[SearchRow](new SearchStatusRenderer)
        case 1 => new Table.LabelRenderer[SearchRow]( row => (NoIcon, row.movies.length.toString) )
        case 2 => new Table.LabelRenderer[SearchRow]( row => (NoIcon, row.term) )
        case 3 => new Table.LabelRenderer[SearchRow]( row => (NoIcon, row.dir) )
        case 4 => new Table.LabelRenderer[SearchRow]( row => (NoIcon, row.file) )
        case 5 => new Table.LabelRenderer[SearchRow]( row => (NoIcon, row.path) )
      }
      rdr.componentFor(this, sel, foc, o, row, col)
    }
  }
  
  val colMdl = tbl.peer.getColumnModel 
  for ( (col,idx) <- cols.zipWithIndex) {
    println(idx + ": "+ col)
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
        publish( SearchRowSelected(row) )
      }
    }
  }
}