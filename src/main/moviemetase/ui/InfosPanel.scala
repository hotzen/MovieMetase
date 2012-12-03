package moviemetase
package ui

import comp._
import scala.swing._
import scala.swing.event._
import java.net.URL
import javax.swing.BoxLayout
import comp.WrapLayout
import javax.swing.JTable
import javax.swing.ListSelectionModel

class InfosPanel(val top: UI) extends MigPanel("fill") {
  
  val postersPanel   = new PostersPanel(top)
  val backdropsPanel = new InfoBackdropsPanel(top)
  val subsPanel      = new SubtitlesPanel(top)
  val websitesPanel  = new WebsitesPanel(top)
  
  val tabbed = new TabbedPane {
    pages += new TabbedPane.Page("Posters", postersPanel, "Poster-Images of selected Movie")
    pages += new TabbedPane.Page("Backdrops", backdropsPanel, "Backdrop-Images of selected Movie")
    pages += new TabbedPane.Page("Subtitles", subsPanel, "Subtitles of selected Movie")
    pages += new TabbedPane.Page("Websites & Trailers", websitesPanel, "Websites of selected Movie")
  }
  add(tabbed, "grow")
  
  listenTo( tabbed )
  reactions += {
    case SelectionChanged(tabbed) => {
      println("InfosPanel selected tab: " + tabbed)
    }
  }
}

//class InfoPostersPanel(val top: UI) extends ScrollPane {
//  val panel = new SeqPanel {
//    peer setLayout new WrapLayout
//  }
//  
//  contents = new ScrollablePanel {
//    peer.setLayout( new BoxLayout(peer, BoxLayout.Y_AXIS) )
//    scrollIncrement = 100
//    contents += panel
//  }
//    
//  def render(infos: List[MovieInfo]): Unit = {
//    panel.clear()
//    
//    val ImageSize = (250, -1)
//    
//    for (poster <- infos.collect({ case i:MovieInfos.Poster => i })) {
//      val comp = new JImage(poster.url, Some(ImageSize)) with JSelectable 
//      panel add comp
//      InfoPostersPanel.this listenTo comp
//    }
//      
//    panel.revalidate()
//  }
//    
//  listenTo( top.searchesPanel )
//  listenTo( top.moviesPanel )
//  
//  reactions += {
//    case SearchSelected(search) => {
//      println("InfoPostersPanel: SearchSelected " + search)
//      search.result.headOption match {
//        case Some(movie) => render( movie.infos )
//        case None =>
//      }
//    }
//      
//    case MovieSelected(movie) => {
//      println("InfoPostersPanel: MovieSelected " + movie)
//      render( movie.infos )
//    }
//    
//    case JSelected(comp) => comp match {
//      case img:JImage => println("InfoPostersPanel selected " + img.url)
//      case _ => println("InfoPostersPanel selected something")
//    }
//    
//    case JUnselected(comp) => comp match {
//      case img:JImage => println("InfoPostersPanelunselected " + img.url)
//      case _ => println("InfoPostersPanel unselected something")
//    }
//  }
//}

class InfoBackdropsPanel(val top: UI) extends ScrollPane {
  val panel = new SeqPanel {
    peer setLayout new WrapLayout
  }
  
  contents = new ScrollablePanel {
    peer.setLayout( new BoxLayout(peer, BoxLayout.Y_AXIS) )
    scrollIncrement = 100
    contents += panel
  }
    
  def render(infos: List[MovieInfo]): Unit = {
    panel.clear()
    
    val ImageSize = (600, -1)
    
    for (backdrop <- infos.collect({ case i:MovieInfos.Backdrop => i })) {
      val comp = new JImage(backdrop.url, Some(ImageSize)) with JSelectable
      panel add comp  
      InfoBackdropsPanel.this listenTo comp
    }
      
    panel.revalidate()
  }
    
  listenTo( top.searchesPanel )
  listenTo( top.moviesPanel )
  
  reactions += {
    case SearchSelected(search) => {
      println("InfoBackdropsPanel: SearchSelected " + search)
      search.result.headOption match {
        case Some(movie) => render( movie.infos )
        case None =>
      }
    }
      
    case MovieSelected(movie) => {
      println("InfoBackdropsPanel: MovieSelected " + movie)
      render( movie.infos )
    }
    
    case JSelected(comp) => comp match {
      case img:JImage => println("InfoBackdropsPanel selected " + img.url)
      case _ => println("InfoBackdropsPanel selected something")
    }
    
    case JUnselected(comp) => comp match {
      case img:JImage => println("InfoBackdropsPanel unselected " + img.url)
      case _ => println("InfoBackdropsPanel unselected something")
    }
  }
}


