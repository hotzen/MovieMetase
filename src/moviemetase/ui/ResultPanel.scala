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
    }, "wrap, height 100!")
    
    // genres
    pnl.add(new MigPanel {
      val genres = movie.infos.collect({ case MovieInfos.Genre(g) => g })

      add(new Label {
        text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
      })
    }, "height 100!")
    
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
    }, "height 100!")
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
