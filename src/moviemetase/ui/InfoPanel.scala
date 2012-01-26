package moviemetase
package ui

import scala.swing._
import scala.swing.event._
import java.net.URL
import javax.swing.BorderFactory
import java.awt.Color

class InfoPanel(val top: UI) extends MigPanel("fill") {
  
  val imagesPanel = new InfoImagesPanel(top)
  val subsPanel   = new InfoSubtitlesPanel(top)
  val sitesPanel  = new InfoWebsitesPanel(top)
  
  val tabbed = new TabbedPane {
    pages += new TabbedPane.Page("Posters & Backdrops", imagesPanel)
    pages += new TabbedPane.Page("Subtitles", subsPanel)
    pages += new TabbedPane.Page("Websites & Trailers", sitesPanel)
  }
  add(tabbed, "grow")
  
  listenTo( top.searchPanel )
  listenTo( tabbed )
  
  reactions += {
    case SelectionChanged(tabbed) => {
      println("selected tab: " + tabbed)
    }
    
    case SearchRowSelected(row) => {
      //panel load renderInfos( row.movies.head.infos )
    }
  }
}

class InfoImagesPanel(val top: UI) extends ScrollPane {
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    
  val posterPanel = new Panel with SequentialContainer.Wrapper {
    peer setLayout new WrapLayout
    
    def add(comp: JImage with Publisher): Unit = {
      contents += Component.wrap( comp )
      InfoImagesPanel.this listenTo comp
    }
  }
  
  val backdropPanel = new Panel with SequentialContainer.Wrapper {
    peer setLayout new WrapLayout
    
    def add(comp: JImage with Publisher): Unit = {
      contents += Component.wrap( comp )
      InfoImagesPanel.this listenTo comp
    }
  }
  
  contents = new BoxPanel(Orientation.Vertical) {
    contents += posterPanel += backdropPanel
  }
  
  val PosterSize = (250, -1)
  val BackdropSize = (600, -1)
  
  def render(infos: List[MovieInfo]): Unit = {
    renderPosters( infos )
    renderBackdrops( infos )
  }
  
  def renderPosters(infos: List[MovieInfo]): Unit =
    for (poster <- infos.collect({ case i:MovieInfos.Poster => i }))
      posterPanel add new JImage(poster.url, Some(PosterSize)) with JSelectable
  
  def renderBackdrops(infos: List[MovieInfo]): Unit =
    for (backdrop <- infos.collect({ case i:MovieInfos.Backdrop => i }))
      backdropPanel add new JImage(backdrop.url, Some(BackdropSize)) with JSelectable
 
  listenTo( top.searchPanel )
  
  reactions += {
    case SearchRowSelected(row) => {
      render( row.movies.head.infos ) // XXX multiple movies??
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
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  
  
}

class InfoWebsitesPanel(val top: UI) extends ScrollPane {
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  
  
}

/*
import java.net.URL
import scala.xml.Elem
import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import org.xhtmlrenderer.simple.XHTMLPanel
import org.xhtmlrenderer.swing.NaiveUserAgent
import org.xhtmlrenderer.extend.UserAgentCallback
import org.xhtmlrenderer.swing.Java2DTextRenderer
import org.xhtmlrenderer.simple.extend.XhtmlNamespaceHandler
import org.xhtmlrenderer.swing.HoverListener
import org.xhtmlrenderer.swing.CursorListener

class InfoPanel(val top: UI) extends ScrollPane {
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  
  import MovieInfos._
  
  def renderInfos(infos: List[MovieInfo]): Elem =
    <html>
      <head>
        <style type="text/css">@import url("/res/common.css") screen;</style>
        <style type="text/css">@import url("/res/infos.css") screen;</style>
      </head>
      <body>
        <div class="posters">
          <h2>Posters</h2>
          { infos.collect({ case i:MovieInfos.Poster => i }).map( renderPoster(_) ) }
        </div>
        <div class="backdrops">
          <h2>Backdrops</h2>
          { infos.collect({ case i:MovieInfos.Backdrop => i }).map( renderBackdrop(_) ) }
        </div>
        <div class="subtitles">
          <h2>Subtitles</h2>
          { infos.collect({ case i:MovieInfos.Subtitle => i }).map( renderSubtitle(_) ) }
        </div>
        <div class="trailers">
          <h2>Trailers</h2>
          { infos.collect({ case i:MovieInfos.Trailer => i }).map( renderTrailer(_) ) }
        </div>
        
        <h2>Other</h2>
        { infos.flatMap( renderInfo(_) ).toList }
      </body>
    </html>;
  
  def renderPoster(poster: MovieInfos.Poster): Elem =
    <a href="select:poster"><img class="selectable" src={ poster.url.toExternalForm } /></a>
  
  def renderBackdrop(backdrop: MovieInfos.Backdrop): Elem =
    <a href="select:backdrop"><img class="selectable" src={ backdrop.url.toExternalForm } /></a>
  
  def renderSubtitle(subtitle: MovieInfos.Subtitle): Elem = <a href={ subtitle.page.toExternalForm } />
  
  def renderTrailer(trailer: MovieInfos.Trailer): Elem = <a href={ trailer.page.toExternalForm } />
  
  
  def renderInfo(info: MovieInfo): Option[Elem] = info match {
    //TODO other infos?
  
    case _ => None
  }
  
  
  val uac = new FS_UserAgent
  val nsh = new XhtmlNamespaceHandler
  val panel = new FS_Panel(uac, nsh)
  
  panel.addMouseTrackingListener( new HoverListener )
  panel.addMouseTrackingListener( new CursorListener )
  panel.addMouseTrackingListener( new FS_MouseListener )
  
  val ctx = panel.getSharedContext()
  ctx.setReplacedElementFactory( new FS_ReplacedElementFactory(panel) )
  ctx.setTextRenderer({
    val r = new Java2DTextRenderer()
    r.setSmoothingThreshold(8)
    r
  })
    
  contents = Component.wrap( panel )
  
  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      panel load renderInfos( row.movies.head.infos )
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
      MovieInfos.Actor("Leonardo DiCaprio", Some("Cobb")) ::
      MovieInfos.Actor("Joseph Gordon-Levitt", Some("Arthur")) ::
      MovieInfos.Director("Christopher Nolan") ::
      MovieInfos.IMDB("tt1375666") :: 
      MovieInfos.TMDB("27205") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      MovieInfos.Poster(new URL("http://ia.media-imdb.com/images/M/MV5BMjAxMzY3NjcxNF5BMl5BanBnXkFtZTcwNTI5OTM0Mw@@._V1._SX640_SY948_.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/bNGehW2wIxagZIlZqszECzXZDck.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/9YL3Frgm8LUYnoWPQXskLYYg5XZ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/2VnwCwBvwYgojCvgEB4wPKsMCvF.jpg") ) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/vmcpt1DALqJSLHTOuN6nJSDzupS.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/hloLHcDVdFoB7eFvLiKpQPqieFP.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/zHl0p6NGVdJgFAEjtEZKhmq7EvM.jpg")) ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}
*/