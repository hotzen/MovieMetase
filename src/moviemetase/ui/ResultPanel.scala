package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.{Desktop, Color, Font}
import javax.swing.{BorderFactory, JButton}
import javax.swing.border.{BevelBorder}

object ResultPanel {
  
}

class ResultPanel(val top: UI) extends ScrollPane {
  
  val panel = new MigPanel("wrap, fillx", "", "")
  contents = panel

  def render(cont: MigPanel, movie: Movie, sel: Boolean, foc: Boolean): Unit = {
    cont.border =
      if (sel)
        BorderFactory.createLineBorder(Color.BLACK)
      else
        BorderFactory.createLineBorder(Color.GRAY)
    
    // title / year  
    cont.add(new Label {
      text = {
        if (movie.year > 0)
          movie.title + " (" + movie.year + ")"
        else
          movie.title
      }
      font = new Font(Font.SERIF, Font.BOLD, 16)
      border = Swing.EmptyBorder

      xAlignment = Alignment.Left
      yAlignment = Alignment.Top
    }, "span, wrap")
    
    // genres
    cont.add(new MigPanel("width 50%") {
      val genres = movie.infos.collect({ case MovieInfos.Genre(g) => g })

      add(new Label {
        text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
      })
    }, "")
    
    // webpages
    cont.add(new MigPanel("width 50%") {
      for (url <- movie.infos.collect({ case i:MovieInfos.WebPage => i.page })) {
        val host = url.getHost 
        val label = 
          if (host startsWith "www.")
            host.substring(4)
          else
            host
        
        val action = new Action(label) {
          def apply() =
            for (desktop <- UI.desktop)
              desktop.browse( url.toURI )
        }
        
        add(new Button(action) {
          contentAreaFilled = true
          borderPainted     = true
          focusPainted      = false
          opaque            = false
          font = new Font(Font.MONOSPACED, Font.PLAIN, 9)
        })
      }
    }, "wrap")
    
//    cont.add(new Separator(), "gapleft rel, growx, span, wrap")
    
    
    if (movie.infos.exists( _ match { 
      case i:MovieInfos.Description => true
      case i:MovieInfos.Summary => true
      case _ => false
    })) {
      // text
      cont.add(new MigPanel("width 95%") {
        
        border = BorderFactory.createSoftBevelBorder(BevelBorder.LOWERED)
        
        for (desc <- movie.infos.collect({ case i:MovieInfos.Description => i.text })) {
          add(new Label {
            text = desc
            font = new Font(Font.SERIF, Font.PLAIN, 11)
            //border = null
            //tabSize = 4
            //editable = false
            //lineWrap = true
            opaque = false
          }, "wrap")
        }
        
        for (sum <- movie.infos.collect({ case i:MovieInfos.Summary => i.text })) {
          add(new Label {
            text = sum
            font = new Font(Font.SERIF, Font.PLAIN, 11)
            border = null
            //tabSize = 4
            //editable = false
            //lineWrap = true
            opaque = false
          }, "wrap")
        }
        
      }, "span, wrap, growx")
    }
  }

  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      panel.clear()
      
      for (movie <- row.movies) {
        val cont = new MigPanel("fill")
        render(cont, movie, false, false)
        panel.add(cont, "grow")
      }
      
      panel.revalidate()
      panel.repaint()
    }
  }
  

  
  
  { // TEST
    val infos =
      MovieInfos.Title("Inception") ::
      MovieInfos.Release(2010) :: 
      MovieInfos.Genre("Drama") ::
      MovieInfos.Genre("SciFi") ::
      MovieInfos.Genre("Doener") ::
      MovieInfos.Genre("Thriller") ::
      MovieInfos.IMDB("tt123456") :: 
      MovieInfos.TMDB("123456") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}