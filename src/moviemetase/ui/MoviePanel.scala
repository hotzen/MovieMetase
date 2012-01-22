package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import org.xhtmlrenderer.simple.XHTMLPanel
import scala.xml.Elem
import org.xhtmlrenderer.swing.NaiveUserAgent
import org.xhtmlrenderer.extend.UserAgentCallback

class MoviePanel(val top: UI) extends FlyingSaucerScrollPane {
  
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
      <h1>
        { movie.title }{ if (movie.year > 0) " (" + movie.year + ")" }
      </h1>
      { for (url <- movie.infos.collect({ case MovieInfos.Poster(url, x) => url }).headOption.toList) yield {
        <img class="main" src={ url.toExternalForm } title={movie.title} />
      }}
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
    </div>
  
  val panel = new XHTMLPanel()
  
  
  
//  val res = new UserAgentCallback {
//    import org.xhtmlrenderer.resource._
//    
//    def getCSSResource(uri: String): CSSResource = {
//      null 
//    }
//     
//    def getImageResource(uri: String): ImageResource = {
//      null
//    }
//    
//    def getXMLResource(String uri): XMLResource = {
//      null
//    }
//  }
  
  val res = new NaiveUserAgent {
    import org.xhtmlrenderer.resource._
    
    override def getCSSResource(uri: String): CSSResource = {
      println("CSS " + uri)
      val is = App.resource(uri).openStream()
//      val is = resolveAndOpenStream(uri)
      new CSSResource(is)
    }
  }
  panel.getSharedContext().setUserAgentCallback( res )
  
  panel.getSharedContext().getTextRenderer().setSmoothingThreshold(8)
  
  contents = Component.wrap( panel )
  
  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      val node = renderMovies( row.movies )
      FlyingSaucer.feedXML(panel, node)
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
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}