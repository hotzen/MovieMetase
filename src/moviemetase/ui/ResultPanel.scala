package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.{Desktop, Color, Font}
import javax.swing.{BorderFactory, JButton}

object ResultPanel {
  def render(pnl: MigPanel, movie: Movie, sel: Boolean, foc: Boolean): Unit = {
    
    println("sel: " + sel + ", foc: " + foc)
    
    pnl.border =
      if (sel)
        BorderFactory.createLineBorder(Color.BLACK)
      else
        BorderFactory.createLineBorder(Color.GRAY)
    
    // title / year  
    pnl.add(new Label {
      text = {
        if (movie.year > 0)
          movie.title + " / " + movie.year
        else
          movie.title + " / ?"
      }
      font = new Font(Font.SERIF, Font.BOLD, 16)

      xAlignment = Alignment.Left
      yAlignment = Alignment.Top
    }, "wrap")
    
    // genres
    pnl.add(new MigPanel {
      val genres = movie.infos.collect({ case MovieInfos.Genre(g) => g })

      add(new Label {
        text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
      })
    }, "")
    
    // webpages
    pnl.add(new MigPanel {
      for (url <- movie.infos.collect({ case p:MovieInfos.WebPage => p.page })) {
        val label = url.getHost
        val action = new Action(label) {
          def apply() = {
            println("AAaaaaaaaaaction " + label)
            println( UI.desktop.toString )
            
            for (desktop <- UI.desktop)
              desktop.browse( url.toURI )
          }
            
        }
        add(new Button(action) {
          borderPainted = false
          opaque = true
          font = new Font(Font.MONOSPACED, Font.PLAIN, 9)
        })
      }
    }, "")
  }
}

class ResultPanelEditor extends javax.swing.table.TableCellEditor {
  import javax.swing.JTable
  import javax.swing.event.CellEditorListener
  import java.util.EventObject
  
  def addCellEditorListener(l: CellEditorListener): Unit = {
    println("addCellEditorListener " + l)
    () 
  }
  def removeCellEditorListener(l: CellEditorListener): Unit = {
    println("removeCellEditorListener " + l)
    () 
  }
   
  def cancelCellEditing(): Unit = {
    println("cancelCellEditing")
    () 
  }
  
  def getCellEditorValue(): AnyRef = {
    println("getCellEditorValue")
    "".asInstanceOf[AnyRef]
  }
  def isCellEditable(e: EventObject): Boolean = {
    println("isCellEditable " + e)
    false
  }
  def shouldSelectCell(e: EventObject): Boolean = {
    println("shouldSelectCell " + e)
    false 
  }
  
  def stopCellEditing(): Boolean = {
    println("stopCellEditing")
    true 
  }
  
  def getTableCellEditorComponent(table: JTable, value: AnyRef, sel: Boolean, row: Int, col: Int): java.awt.Component = {
    println("getTableCellEditorComponent " + value + " sel:" + sel + " row:" + row + " col:" + col)
    null
  }
}


class ResultPanel(val top: UI) extends ScrollPane {
    
  val mdl = new ArrayTableModel[Movie]
  
  contents = new Table {
    model = mdl
    autoResizeMode = Table.AutoResizeMode.AllColumns
    
    rowHeight = 200
    
    opaque = true
    foreground = null
    background = null
    
    selectionForeground = null
    selectionBackground = null
    
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
      val r = peer.convertRowIndexToModel(row)
      val c = peer.convertColumnIndexToModel(col)
      val m = mdl.rows(r)
      
      val rdr = new MigPanelRenderer[Movie](ResultPanel.render _, "", "", "")
      rdr.componentFor(this, sel, foc, m, row, col)
    }
    
    val cellEditor = new ResultPanelEditor
    override protected def editor(row: Int, col: Int): javax.swing.table.TableCellEditor = {
      println("editor row:" + row + " col:" + col)
      cellEditor
    }

    peer.setTableHeader(null)
  }
  
  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      mdl.rows.clear()
      mdl.rows appendAll row.movies
      mdl.fireTableDataChanged()
    }
  }
}
