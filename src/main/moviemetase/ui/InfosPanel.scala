package moviemetase
package ui

import scala.swing._
import scala.swing.event._
import java.net.URL
import javax.swing.BoxLayout
import moviemetase.ui.comp.WrapLayout
import comp._
import javax.swing.JTable
import javax.swing.ListSelectionModel

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
      println("InfosPanel selected tab: " + tabbed)
    }
  }
}

class InfoPostersPanel(val top: UI) extends ScrollPane {
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
    
    val ImageSize = (250, -1)
    
    for (poster <- infos.collect({ case i:MovieInfos.Poster => i })) {
      val comp = new JImage(poster.url, Some(ImageSize)) with JSelectable 
      panel add comp
      InfoPostersPanel.this listenTo comp
    }
      
    panel.revalidate()
  }
    
  listenTo( top.searchesPanel )
  listenTo( top.moviesPanel )
  
  reactions += {
    case SearchSelected(search) => {
      println("InfoPostersPanel: SearchSelected " + search)
      search.result.headOption match {
        case Some(movie) => render( movie.infos )
        case None =>
      }
    }
      
    case MovieSelected(movie) => {
      println("InfoPostersPanel: MovieSelected " + movie)
      render( movie.infos )
    }
    
    case JSelected(comp) => comp match {
      case img:JImage => println("InfoPostersPanel selected " + img.url)
      case _ => println("InfoPostersPanel selected something")
    }
    
    case JUnselected(comp) => comp match {
      case img:JImage => println("InfoPostersPanelunselected " + img.url)
      case _ => println("InfoPostersPanel unselected something")
    }
  }
}

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
class InfoSubtitlesPanel(val top: UI) extends ScrollPane {
  
  import MovieInfos.Subtitle
  
  val cols =
    new TableCol("Label", 200) ::
    new TableCol("Release", 200) ::
    new TableCol("Language", 200) ::
    new TableCol("Page", 100) ::
    new TableCol("Download", 100)  ::
    Nil

  val model = new TableModel[Subtitle](cols) {
    def getValue(sub: Subtitle, col: Int): AnyRef = col match {
      case 0 => sub.label
      case 1 => sub.releaseText.getOrElse("")
      case 2 => sub.langText
      case 3 => sub.website.toExternalForm
      case 4 => sub.download.map(_.toExternalForm).getOrElse("")
    }
  }
  
  val table = new JTable(model)
  table setTableHeader null
  table setSelectionMode ListSelectionModel.SINGLE_INTERVAL_SELECTION //SINGLE_SELECTION
  
  cols.zipWithIndex.foreach({ case (col, idx) =>
    val colModel = table.getColumnModel.getColumn(idx) 
    colModel setPreferredWidth col.prefWidth
    if (col.maxWidth > 0)
      colModel setMaxWidth col.maxWidth
  })
  table setRowHeight 35
  
  val downloadAction = new Action({evt =>
    val rowIdx = evt.getActionCommand.toInt
    val search = model.rows( rowIdx )
    //val f = new java.io.File( search.fileInfo.dirPath )
    //UI.openFile(f)
  })
  new ButtonColumn(table, downloadAction, 3)
  
  contents = Component wrap table  
    
  table.getSelectionModel.addListSelectionListener(OnListSelectedIndex(selIdx => {
    val selSearch = model.rows( selIdx )
    //SubtitlesPanel.this.publish( SearchSelected(selSearch) )
  }))
  
  def updateEq(a: Subtitle, b:Subtitle): Boolean = a.website == b.website
  def updateModel = model.update(updateEq) _

  listenTo( top.dropPanel )
//  reactions += {
//    case FoundMovieFile(fileInfo) => {
//      updateModel( Search(fileInfo, SearchStatus.Pending) )
//    }
//    case SearchingMoviesByFile(fileInfo) => {
//      updateModel( Search(fileInfo, SearchStatus.Searching) )
//    }
//    case FoundMoviesByFile(fileInfo, movies) => {
//      updateModel( Search(fileInfo, SearchStatus.Completed, movies) )
//    }
//    case SearchingMoviesByFileFailed(fileInfo, t) => {
//      updateModel( Search(fileInfo, SearchStatus.Failed, Nil, Some(t)) )
//    }
//  }
}

class InfoWebsitesPanel(val top: UI) extends ScrollPane {

}