package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.{Desktop, Color, Font}
import javax.swing.{BorderFactory, JButton}

object ResultPanel {
  
}

class ResultPanel(val top: UI) extends ScrollPane {
    
  val panel = new MigPanel("wrap", "", "")
  contents = panel
  
  val WWW = "www."
  
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
    }, "wrap")
    
    // genres
    cont.add(new MigPanel {
      val genres = movie.infos.collect({ case MovieInfos.Genre(g) => g })

      add(new Label {
        text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
      })
    }, "")
    
    // webpages
    cont.add(new MigPanel {
      for (url <- movie.infos.collect({ case p:MovieInfos.WebPage => p.page })) {
        val host = url.getHost 
        val label = 
          if (host startsWith WWW)
            host.substring(WWW.length)
          else
            host
        
        val action = new Action(label) {
          def apply() =
            for (desktop <- UI.desktop)
              desktop.browse( url.toURI )
        }
        
        add(new Button(action) {
          contentAreaFilled = false
          borderPainted     = false
          focusPainted      = true
          opaque            = false
          font = new Font(Font.MONOSPACED, Font.PLAIN, 9)
        })
      }
    }, "")
  }

  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      panel.clear()
      
      for (movie <- row.movies) {
        // create container
        val cont = new MigPanel() { }
        panel.add(cont, "grow")
    
        // render into container
        render(cont, movie, false, false)
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
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}