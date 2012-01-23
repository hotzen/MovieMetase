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

class MoviePanel(val top: UI) extends FS_ScrollPane {
  
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
      
  def renderMovies(movies: List[Movie]): Elem =
    <html>
      <head>
        <style type="text/css">@import url("/res/common.css") screen;</style>
        <style type="text/css">@import url("/res/movies.css") screen;</style>
      </head>
      <body>
      { movies.map( renderMovie(_) ) }
      </body>
    </html>;
    
  def renderMovie(movie: Movie): Elem =
    <div class="movie">
      { for (url <- movie.infos.collect({ case MovieInfos.Poster(url, x) => url }).headOption.toList) yield {
        <img class="main" src={ url.toExternalForm } title={ movie.title } />
      }}
      <h1>
        { movie.title }{ if (movie.year > 0) " (" + movie.year + ")" }
      </h1>
      <div class="genres-directors-actors">
        <span class="genres">
          { movie.infos.collect({ case MovieInfos.Genre(genre) => genre }).mkString("Genres: ", ", ", "") }
        </span>
        <span class="directors">
          { movie.infos.collect({ case MovieInfos.Director(director) => director }).mkString("Director: ", ", ", "") }
        </span>
        <span class="actors">
          { movie.infos.collect({ case MovieInfos.Actor(name, x) => name }).mkString("Actors: ", ", ", "") }
        </span>
      </div>
      {for (desc <- movie.infos.collect({ case MovieInfos.Description(text) => text })) yield {
        <blockquote class="description">\u00AB { desc } \u00BB</blockquote>
      }}
      {for (summary <- movie.infos.collect({ case MovieInfos.Summary(text) => text })) yield {
        <blockquote class="summary">\u00AB { summary } \u00BB</blockquote>
      }}
      <div class="webpages">
        { for (url <- movie.infos.collect({ case i:MovieInfos.WebPage => i.page })) yield {
          val host = url.getHost
          val label = if (host startsWith "www.") host.substring(4)
                      else host
          <a href={ url.toExternalForm }>{ label }</a>
        }}
      </div>
      <div style="clear: both;" />
    </div>
  
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
      panel load renderMovies( row.movies )
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
      //MovieInfos.Description("Test") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      MovieInfos.Poster(new URL("http://ia.media-imdb.com/images/M/MV5BMjAxMzY3NjcxNF5BMl5BanBnXkFtZTcwNTI5OTM0Mw@@._V1._SX640_SY948_.jpg") ) ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}