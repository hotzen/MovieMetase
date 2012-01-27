package moviemetase
package ui

import scala.swing._
import scala.swing.event._
import java.net.URL
import javax.swing.BoxLayout

class InfosPanel(val top: UI) extends MigPanel("fill") {
  
  val postersPanel   = new InfoPostersPanel(top)
  val backdropsPanel = new InfoBackdropsPanel(top)
  val subsPanel      = new InfoSubtitlesPanel(top)
  val sitesPanel     = new InfoWebsitesPanel(top)
  
  val tabbed = new TabbedPane {
    pages += new TabbedPane.Page("Posters", postersPanel)
    pages += new TabbedPane.Page("Backdrops", backdropsPanel)
    pages += new TabbedPane.Page("Subtitles", subsPanel)
    pages += new TabbedPane.Page("Websites & Trailers", sitesPanel)
  }
  add(tabbed, "grow")
  
  listenTo( tabbed )
  reactions += {
    case SelectionChanged(tabbed) => {
      println("selected tab: " + tabbed)
    }
  }
}

class InfoPostersPanel(val top: UI) extends ScrollPane {
  
  val ImageSize = (250, -1)
      
  val panel = new Panel with SequentialContainer.Wrapper {
    peer setLayout new WrapLayout
    
    def add(comp: JImage with Publisher): Unit = {
      contents += Component.wrap( comp )
      InfoPostersPanel.this listenTo comp
    }
    def clear(): Unit = contents.clear()
  }
    
  val backdropPanel = new Panel with SequentialContainer.Wrapper {
    peer setLayout new WrapLayout
    
    def add(comp: JImage with Publisher): Unit = {
      contents += Component.wrap( comp )
      InfoPostersPanel.this listenTo comp
    }
    def clear(): Unit = contents.clear()
  }
  
  contents = new ScrollablePanel {
    peer.setLayout( new BoxLayout(peer, BoxLayout.Y_AXIS) )
    contents += panel
  }
    
  def render(infos: List[MovieInfo]): Unit = {
    panel.clear()
    
    for (poster <- infos.collect({ case i:MovieInfos.Poster => i }))
      panel add new JImage(poster.url, Some(ImageSize)) with JSelectable
      
    panel.revalidate()
  }
    
  listenTo( top.searchesPanel )
  listenTo( top.moviesPanel )
  
  reactions += {
    case SearchSelected(row) => {
      println("InfosPanel: SearchSelected " + row)
      row.movies.headOption match {
        case Some(movie) => render( movie.infos )
        case None =>
      }
    }
      
    case MovieSelected(movie) => {
      println("InfosPanel: MovieSelected " + movie)
      render( movie.infos )
    }
    
    case JSelected(comp) => comp match {
      case img:JImage => println("selected " + img.url)
      case _ => println("selected something")
    }
    
    case JUnselected(comp) => comp match {
      case img:JImage => println("unselected " + img.url)
      case _ => println("unselected something")
    }
  }
}

class InfoBackdropsPanel(val top: UI) extends ScrollPane {

  val ImageSize = (600, -1)
      
  val panel = new Panel with SequentialContainer.Wrapper {
    peer setLayout new WrapLayout
    
    def add(comp: JImage with Publisher): Unit = {
      contents += Component.wrap( comp )
      InfoBackdropsPanel.this listenTo comp
    }
    def clear(): Unit = contents.clear()
  }
  
  contents = new ScrollablePanel {
    peer.setLayout( new BoxLayout(peer, BoxLayout.Y_AXIS) )
    contents += panel
  }
    
  def render(infos: List[MovieInfo]): Unit = {
    panel.clear()
    
    for (backdrop <- infos.collect({ case i:MovieInfos.Backdrop => i }))
      panel add new JImage(backdrop.url, Some(ImageSize)) with JSelectable
      
    panel.revalidate()
  }
    
  listenTo( top.searchesPanel )
  listenTo( top.moviesPanel )
  
  reactions += {
    case SearchSelected(row) => {
      println("InfosPanel: SearchSelected " + row)
      row.movies.headOption match {
        case Some(movie) => render( movie.infos )
        case None =>
      }
    }
      
    case MovieSelected(movie) => {
      println("InfosPanel: MovieSelected " + movie)
      render( movie.infos )
    }
    
    case JSelected(comp) => comp match {
      case img:JImage => println("selected " + img.url)
      case _ => println("selected something")
    }
    
    case JUnselected(comp) => comp match {
      case img:JImage => println("unselected " + img.url)
      case _ => println("unselected something")
    }
  }
}
class InfoSubtitlesPanel(val top: UI) extends ScrollPane {

}

class InfoWebsitesPanel(val top: UI) extends ScrollPane {

}