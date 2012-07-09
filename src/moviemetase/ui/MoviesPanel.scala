package moviemetase
package ui

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
import org.w3c.dom.Element
import org.xhtmlrenderer.swing.BasicPanel

case class MovieSelected(movie: Movie) extends Event

class MoviesPanel(val top: UI) extends FS_ScrollPane {
  
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
      
  def renderMovies(movies: List[Movie]): Elem =
    <html>
      <head>
        <style type="text/css">@import url("/res/movies.css") screen;</style>
      </head>
      <body>
      { movies.map( renderMovie(_) ) }
      </body>
    </html>;

  def renderMovie(movie: Movie): Elem =
    <div class="movie" select={ movie.id }>
      { for (url <- movie.infos.collect({ case MovieInfos.Poster(url, x) => url }).headOption.toList) yield {
        <img class="main" src={ url.toExternalForm } title={ movie.title } />
      }}
      <h1>
        { movie.title }{ if (movie.year > 0) " (" + movie.year + ")" }
      </h1>
      <span class="websites">
        { for (info <- movie.infos.collect({ case i:MovieInfos.Website => i })) yield {
          val url = info.page
          val host = url.getHost
          val label = if (host startsWith "www.") host.substring(4)
                      else host

          <a href={ url.toExternalForm }>{ label }</a>
        }}
      </span>
      <div class="genres-directors-actors">

        { val genres = movie.infos.collect({ case i:MovieInfos.Genre => i })
          if (!genres.isEmpty) {
            <span class="genres">
              <span class="label">Genres:</span>
              { for (info <- genres) yield {
                <span class="genre">{ info.name }</span>
              }}
            </span>
          }
        }
                
        { val directors = movie.infos.collect({ case i:MovieInfos.Director => i })
          if (!directors.isEmpty) {
            <span class="directors">
              <span class="label">Director:</span>
              { for (info <- directors ) yield {
                <span class="director">{ info.name }</span>
              }}
            </span>
          }
        }
        
        { val actors = movie.infos.collect({ case i:MovieInfos.Actor => i })
          if (!actors.isEmpty) {
            <span class="actors">
              <span class="label">Actors:</span>
              { for (info <- actors) yield {
                <span class="actor">{ info.name }</span>
              }}
            </span>
          }
        }
        
      </div>
      {for (info <- movie.infos.collect({ case i:MovieInfos.Tagline => i})) yield {
        <blockquote class="description">\u00AB { info.text } \u00BB</blockquote>
      }}
      /*{for (info <- movie.infos.collect({ case i:MovieInfos.Summary => i })) yield {
        <blockquote class="summary">\u00AB { info.text } \u00BB</blockquote>
      }}*/
      <div style="clear: both;" />
    </div>
  
  //def selector(m: Movie, i: MovieInfo): String = m.id + "/" + i.id

  val movies = scala.collection.mutable.Map[String, Movie]()
        
  val uac = new FS_UserAgent
  val nsh = new XhtmlNamespaceHandler
  val panel = new FS_Panel(uac, nsh)
    
  panel.addMouseTrackingListener( new HoverListener )
  panel.addMouseTrackingListener( new CursorListener )
  panel.addMouseTrackingListener( new FS_MouseListener {
    
    override def select(panel: FS_Panel, id: String, elem: Element): Unit = {
      val nsh = panel.getSharedContext.getNamespaceHandler

      toggleCssClass(elem, "selected", nsh)
      
      panel.reload()
      panel.relayout()
      
      println("MoviesPanel.select(" + id + ")")
      
      movies.get(id) match {
        case Some(movie) => MoviesPanel.this.publish( MovieSelected(movie) ) 
        case None => println("no movie")
      }
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
  
  listenTo( top.searchesPanel )
  reactions += {
    case SearchSelected(row) => {
      movies.clear()
      
      for (movie <- row.movies)
        movies put (movie.id, movie)
      
      panel load renderMovies( row.movies )
    }
  }
}