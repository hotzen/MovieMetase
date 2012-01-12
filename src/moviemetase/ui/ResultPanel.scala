package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.{Desktop, Color, Font}
import javax.swing.{BorderFactory, JButton}
import scala.swing.Table.LabelRenderer

class ResultPanel(val top: UI) extends ScrollPane {
    
  val mdl = new ArrayTableModel[Movie]
  val rdr = new TableRenderer[Movie](new ResultRenderer)
  
  contents = new Table {
    model = mdl
    autoResizeMode = Table.AutoResizeMode.AllColumns
    
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
      val r = peer.convertRowIndexToModel(row)
      val c = peer.convertColumnIndexToModel(col)
      val m = mdl.rows(r)
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

class ResultRenderer extends MigPanel("wrap,fill", "[grow,fill]", "[][grow,fill][]") with TableRendererComp[Movie] {

  def render(m: Movie, sel: Boolean, foc: Boolean) {
    border =
      if (sel)
        BorderFactory.createLineBorder(Color.BLACK)
      else
        BorderFactory.createLineBorder(Color.GRAY)
        
    val headerPnl = new MigPanel {
      
      // title / year  
      add(new Label {
        text = {
          if (m.year > 0)
            m.title + " / " + m.year
          else
            m.title + " / ?"
        }
        font = new Font(Font.SERIF, Font.BOLD, 16)
  
        xAlignment = Alignment.Left
        yAlignment = Alignment.Top
      }, "wrap")
      
      // genres
      add(new MigPanel {
        val genres = m.infos.collect({ case MovieInfos.Genre(g) => g })
  
        add(new Label {
          text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
          font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
        })
      })
      
      // webpages
      add(new MigPanel {
        for (url <- m.infos.collect({ case p:MovieInfos.WebPage => p.page })) {
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
      })
    }
    
    val contentPnl = new MigPanel {
      
    }
    
    val footerLbl = new Label {
      text = "footer"
    }
    
    add(headerPnl, "")
    add(contentPnl, "grow")
    add(footerLbl, "height 50!")
  }
}

