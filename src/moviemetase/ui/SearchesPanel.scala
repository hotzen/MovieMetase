package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.swing.Icon
import org.xhtmlrenderer.swing.BasicPanel
import org.xhtmlrenderer.swing.NaiveUserAgent
import org.xhtmlrenderer.swing.Java2DTextRenderer
import org.xhtmlrenderer.swing.HoverListener
import org.xhtmlrenderer.swing.CursorListener
import org.xhtmlrenderer.simple.XHTMLPanel
import org.xhtmlrenderer.simple.extend.XhtmlNamespaceHandler
import org.xhtmlrenderer.extend.UserAgentCallback
import scala.xml.Elem
import org.w3c.dom.Element
import comp._
import scala.collection.mutable.ListBuffer

case class SearchSelected(search: Search) extends Event

case class Search(fileInfo: FileInfo, completed: Boolean, result: List[Movie])

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
        
    <div class={ clz }>
      <img src={ img } />
      <div class="dir" select={ search.fileInfo.dirName }>{ search.fileInfo.dirName }</div>
      <div class="file" select={ search.fileInfo.dirName }>{ search.fileInfo.fileName }</div>
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

      toggleCssClass(elem, "selected", nsh)
      println("SearchesPanel.select(" + id + ")")
      
      panel.reload()
      panel.relayout()
      
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