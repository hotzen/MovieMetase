package moviemetase
package ui

//import scala.swing._
//import scala.swing.Swing._
import javax.swing._
import java.awt.{Component => JComponent}
import scala.xml.Elem
import org.w3c.dom.Element
import scala.collection.mutable._
import javax.swing._
import javax.swing.event._
import scala.swing.event.Event
import comp._
import scala.swing.ScrollPane
import scala.swing.Component
import java.awt.Color
import javax.swing.border.EtchedBorder

case class SearchSelected(search: Search) extends Event

case class Search(fileInfo: FileInfo, completed: Boolean, result: List[Movie]) {
  override def toString() = {
    val sb = new StringBuilder
    sb append "["
    sb append { if (completed) "X" else " " }
    sb append "] "
    sb append fileInfo.dirName
    sb append " /\n"
    sb append fileInfo.fileName
    sb.toString
  }
}

class SearchesPanel(val top: UI) extends ScrollPane {
  import language.reflectiveCalls
  
  var searches = List[Search]()
  
  val listModel = new javax.swing.AbstractListModel[Search] {
    def getElementAt(n: Int) = searches(n)
    def getSize = searches.length
    
    def addPending(fileInfo: FileInfo) {
      val newSearch = Search(fileInfo, false, Nil)
      searches = newSearch :: searches
      fireIntervalAdded(newSearch, 0, 0)
    }
    
    def addCompleted(fileInfo: FileInfo, movies: List[Movie]) {
      val newSearch = Search(fileInfo, true, movies)
      
      val parted: (List[(Search,Int)], List[(Search,Int)]) =
        searches.zipWithIndex.partition({ case (search, idx) => search.fileInfo != newSearch.fileInfo })

      val (keepTpl, removeTpl) = parted
      
      searches = keepTpl.map( _._1 )
      
      for ((search, idx) <- removeTpl)
        fireIntervalRemoved(search, idx, idx)
      
      searches = newSearch :: searches
      fireIntervalAdded(newSearch, 0, 0)
    }
  }
  
  val list = new JList(listModel)
  list setSelectionMode ListSelectionModel.SINGLE_SELECTION
  
  list.addListSelectionListener(OnListSelectedEvent(evt => {
    val idx = evt.getFirstIndex
    if (idx < searches.length) {
      val selSearch = searches( idx )
      SearchesPanel.this.publish( SearchSelected(selSearch) )
    }
  }))
  
  list.setCellRenderer(ListLabelRenderer( (lbl, idx, selected) => {
    if (idx < searches.length) {
      val search = searches(idx)
          
      lbl setOpaque true
      
      (search.completed, search.result.length) match {
        case (true, 0) => {
          lbl.setIcon( new ImageIcon(App.resource("/res/fail.png")) )
        }
        case (true, 1) => {
          lbl.setIcon( new ImageIcon(App.resource("/res/ok.png")) )
        }
        case (true, _) => {
          lbl.setIcon( new ImageIcon(App.resource("/res/plus.png")) )
        }
        case (false, _) => {
          lbl.setIcon( new ImageIcon(App.resource("/res/clouds.png")) )
        }
      }
      
      if (selected) {
        lbl.setBackground( Color.LIGHT_GRAY )
        lbl.setBorder( BorderFactory.createLineBorder(Color.BLUE) )
      } else {
        lbl.setBackground( Color.WHITE )
        lbl.setBorder( BorderFactory.createLineBorder(Color.BLACK) )
      }
      
      val sb = new StringBuilder
      sb append "<html>"
      sb append search.fileInfo.dirName
      sb append "<br>"
      sb append "<strong>/ </strong>"
      sb append search.fileInfo.fileName
      sb append "</html>"
      
      lbl.setText(sb.toString)
    }
  }))
  
  contents = Component wrap list  

  listenTo( top.dropPanel )
  reactions += {
    case SearchingMoviesByFile(fileInfo) =>
      listModel.addPending(fileInfo)
    
    case FoundMoviesByFile(fileInfo, movies) =>
      listModel.addCompleted(fileInfo, movies)
  }
}

case class ListLabelRenderer[A](f: (JLabel, Int, Boolean) => Unit) extends ListCellRenderer[A] {
  val lbl = new JLabel 
  def getListCellRendererComponent(list: JList[_ <: A], value: A, idx: Int, selected: Boolean, focused: Boolean): JComponent = {
    f(lbl, idx, selected)
    lbl
  }
}

case class OnListSelectedEvent(f: ListSelectionEvent => Unit) extends ListSelectionListener {
  def valueChanged(evt: ListSelectionEvent) {
    if (!evt.getValueIsAdjusting)
      f( evt )
  }
}


/*
class SearchesPanel(val top: UI) extends FS_ScrollPane {
  
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    
  def renderSearches(searches: List[Search]): Elem =
    <html>
      <head>
        <style type="text/css">@import url("/res/searches.css") screen;</style>
      </head>
      <body>
      { searches.map( renderSearch(_) ) }
      </body>
    </html>;
    
  def renderSearch(search: Search): Elem = {
    val (clz, img) = (search.completed, search.result.length) match {
      case (false,  _) => ("search pending", "/res/clouds.png")
      case (true, 0)   => ("search failed",  "/res/fail.png")
      case (true, 1)   => ("search found",   "/res/ok.png")
      case (true, _)   => ("search matches", "/res/plus.png")
    }
        
    <div class={ clz } select={ "SEL_" + search.fileInfo.path  }>
      <img src={ img } />
      <div class="dir" select={ "OPEN_" + search.fileInfo.dirPath }>{ search.fileInfo.dirName }</div>
      <div class="file">/ { search.fileInfo.fileName }</div>
    </div>
  }
    
  val uac = new FS_UserAgent
  val nsh = new XhtmlNamespaceHandler
  val panel = new FS_Panel(uac, nsh)
    
  panel.addMouseTrackingListener( new HoverListener )
  panel.addMouseTrackingListener( new CursorListener )
  panel.addMouseTrackingListener( new FS_MouseListener {
    
  override def select(panel: FS_Panel, id: String, elem: Element): Unit = {
    val nsh = panel.getSharedContext.getNamespaceHandler
      
    println("SearchesPanel.select(" + id + ")")
    
    if (id.startsWith("SEL_")) {
      toggleCssClass(elem, "selected", nsh)
      panel.reload()
      panel.relayout()
      
      val selPath = id.substring(4)
      println("selected: " + selPath)
      
    } else if (id.startsWith("OPEN_")) {
      try {
        val f = new java.io.File( id.substring(5) )
        UI.desktop match {
          case Some(dsk) => dsk.open(f)
          case None =>
        }
      } catch {
        case e:Exception =>
      }
    }
      
//      movies.get(id) match {
//        case Some(movie) => MoviesPanel.this.publish( MovieSelected(movie) ) 
//        case None => println("no movie")
//      }
    }
  })
  
  val ctx = panel.getSharedContext()
  ctx.setReplacedElementFactory( new FS_ReplacedElementFactory(panel) )
  ctx.setTextRenderer({
    val r = new Java2DTextRenderer()
    r.setSmoothingThreshold(8)
    r
  })
  
  contents = Component.wrap( panel )
  panel load renderSearches(Nil)
  
  val searches = new ListBuffer[Search]()
    
  //listenTo( tbl.selection )
  listenTo( top.dropPanel )
  
  reactions += {
    case SearchingMoviesByFile(fileInfo) => {
      println("SearchingMoviesByFile")
      
      searches append Search(fileInfo, false, Nil)
      panel load renderSearches(searches.toList)
    }

    case FoundMoviesByFile(fileInfo, movies) => {
//      mdl remove (_.path == file.path)
//      mdl add SearchRow(false, "", file.dirName, file.fileName, file.path, movies)
    }
  
    case TableRowsSelected(src, rng, false) => {
//      for (rowIdx <- src.selection.rows) {
//        val row = mdl.rows(rowIdx)
//        publish( SearchSelected(row) )
//      }
    }
  }
}
*/