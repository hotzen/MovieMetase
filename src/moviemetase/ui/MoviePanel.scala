package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import org.xhtmlrenderer.simple.XHTMLPanel
import scala.xml.Elem
import org.xhtmlrenderer.swing.NaiveUserAgent
import org.xhtmlrenderer.extend.UserAgentCallback
import java.net.URL
import org.xhtmlrenderer.swing.Java2DTextRenderer
import org.xhtmlrenderer.simple.extend.XhtmlCssOnlyNamespaceHandler
import org.xhtmlrenderer.simple.extend.XhtmlNamespaceHandler

class MoviePanel(val top: UI) extends FS_ScrollPane {
  
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
      
  def renderMovies(movies: List[Movie]): Elem =
    <html>
      <head>
        <title>test</title>
        <!--<link rel="stylesheet" href="/res/movies.css" type="text/css" media="all" />-->
        <!--<link rel="stylesheet" href="/res/movies.css" type="text/css" media="screen" /> -->
        <style type="text/css"> @import url("/res/movies.css") screen; </style>
      </head>
      <body>
      { movies.map( renderMovie(_) ) }
      </body>
    </html>;
    
  def renderMovie(movie: Movie): Elem =
    <div class="movie">
      { for (url <- movie.infos.collect({ case MovieInfos.Poster(url, x) => url }).headOption.toList) yield {
        <img class="main" src={ url.toExternalForm } title={movie.title} />
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
  //val nsh = new XhtmlCssOnlyNamespaceHandler
  val nsh = new XhtmlNamespaceHandler
        
  val panel = new FS_Panel(uac, nsh)
  //panel.addMouseTrackingListener( new FS_MouseListener )
  
  //val ctx = panel.getSharedContext()
  //ctx.setUserAgentCallback( new FS_UserAgent )
  panel.ctx.setReplacedElementFactory( new FS_ReplacedElementFactory(panel) )
  panel.ctx.setTextRenderer({
    val r = new Java2DTextRenderer()
    r.setSmoothingThreshold(8)
    r
  })
    
  contents = Component.wrap( panel )
  
  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      panel load renderMovies( row.movies )
      
      //FlyingSaucer.feedXML(panel, node)
//    panel.revalidate()
//    panel.repaint()
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
      MovieInfos.IMDB("tt123456") :: 
      MovieInfos.TMDB("123456") ::
      //MovieInfos.Description("Test") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      MovieInfos.Poster(new URL("http://ia.media-imdb.com/images/M/MV5BMjAxMzY3NjcxNF5BMl5BanBnXkFtZTcwNTI5OTM0Mw@@._V1._SX640_SY948_.jpg") ) ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}