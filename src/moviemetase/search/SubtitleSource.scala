package moviemetase
package search

import query._
import Util._
import java.net.URL
import java.io.InputStream
import nu.xom._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.util.concurrent.Future

sealed trait SubtitleSourceQuery extends Query[List[Movie]] with UrlTask[List[Movie]]

object SubtitleSource {
  
  val BaseUrl = "http://www.subtitlesource.org"
  
  val ReleaseCSE = "011282045967305256347:2mp-i5b1ixi"
  
  object Regex {
    val MovieLink       = """/title/(tt\d+)""".r // yay, an IMDB-ID!
    val ReleaseLink     = """/release/(\d+)[^"]*""".r
    val SubtitleLink    = """/subs/(\d+)[^"]*""".r  
    val DownloadZipLink = """/download/zip/(\d+)""".r
  }
    
  case class ReleaseSearch(val id: String) extends SearchStrategy[List[MovieInfo]] with Logging {
    val logID = "SubtitleSource_ReleaseSearch(" + id  + ")"
    
    val BaseScore = 0.95
    
    def search(term: String): List[List[MovieInfo]] = {
      val fuzzy = Google.fuzzyTerm(term)
      trace("querying GoogleCSE with fuzzy term '" + fuzzy + "'")
      
      val relQuery = GoogleCSE.Query(ReleaseCSE, term)
      val fut = relQuery.execute()
      val res = fut.get() // block

      // extract Release-Page
      val releaseFuts =
        for ( (r, idx) <- res.zipWithIndex;
               _       <- Regex.ReleaseLink.findFirstIn(r.url.toString) ) yield {

          val score = BaseScore * (1.0 - idx * 0.05)
          trace("found ReleasePage", ("ResultIndex" -> idx) :: ("ReleasePage" -> r.url) :: ("Score" -> score) :: Nil)
          
          val extract = ReleasePageExtractor( r.url )
          extract.logOut = logOut
          extract.execute()
        }
      
      // join Futures
      val allReleaseInfos = for ( fut  <- releaseFuts; link <- fut.get() ) yield link      

      // filter out duplicate Subtitle-Pages
      val releaseInfos = allReleaseInfos.distinctNoCount((a,b) => a.subtitlePage == b.subtitlePage) // map out counts //.sortByCount().noCount()
            
      // extract Subtitle-page
      val combinedFuts =
        for ( releaseInfo <- releaseInfos ) yield {
          val extract = SubtitlePageExtractor( releaseInfo.subtitlePage )
          extract.logOut = logOut
          (releaseInfo, extract.execute())
        }
      
      // join Futures
      val combinedInfos =
        for ( (releaseInfo, fut) <- combinedFuts) yield 
          (releaseInfo, fut.get())
      
      // generate MovieInfo
      val infos =
        for ( (releaseInfo,subInfoOpt) <- combinedInfos; subInfo <- subInfoOpt) yield {
          val movieInfos = new ListBuffer[MovieInfo]()
          // ReleasePageInfo(releasePage: URL, subtitlePage: URL, label: String, lang: String)
          // SubtitlePageInfo(subtitlePage: URL, downloadUrl: URL, moviePage: Option[MoviePageInfo])
          // MoviePageInfo(moviePage: URL, imdbID: String)
  
          movieInfos append MovieInfos.Subtitle(
            releaseInfo.label,
            releaseInfo.lang,
            subInfo.subtitlePage,
            subInfo.downloadUrl
          )//.withSourceInfo( subInfo.subtitlePage.toString )
          
          if (!subInfo.moviePage.isEmpty) {
            val moviePage = subInfo.moviePage.get
            movieInfos append MovieInfos.Imdb(
              moviePage.imdbID
            ).withSourceInfo( moviePage.moviePage.toString )
          }
          
          movieInfos.toList
        }
      
      infos.toList
    }
  }
  
  case class ReleasePageInfo(releasePage: URL, subtitlePage: URL, label: String, lang: String)
  
  case class ReleasePageExtractor(url: URL) extends HtmlTask[List[ReleasePageInfo]] with Logging {
    import XOM._
    val logID = "SubtitleSource_ReleasePageExtractor(" +  url.toString + ")"

    def process(doc: nu.xom.Document): List[ReleasePageInfo] = {
      val infos = new ListBuffer[ReleasePageInfo]
      val ctx = Some( XOM.XPath.XHTML )
      
      // all links
      for (aNode <- doc.xpath("""//xhtml:li/xhtml:a[@href]""", ctx);
           aElem <- aNode.toElement) {
      
        // only links to subtitle-pages
        for (href <- aElem.attribute("href");
             m    <- Regex.SubtitleLink.findFirstIn(href)) {
          
           val subtitlePage = BaseUrl + m
           val label = aElem.getValue.trim
           
           val langList = 
             for (imgNode <- aElem.xpath("""xhtml:img[@alt]""", ctx);
                  imgElem <- imgNode.toElement;
                  alt     <- imgElem.attribute("alt"))
             yield alt.trim
             
           val lang = langList.headOption.getOrElse("").trim.toLowerCase
           
           val info = ReleasePageInfo(url, subtitlePage.toURL, label, lang)
           trace("SubtitlePage=" + subtitlePage + "; Label=" + label + "; Lang=" + lang)
           infos append info
        }
      }
      
      infos.toList
    }
  }
  
  case class SubtitlePageInfo(subtitlePage: URL, downloadUrl: URL, moviePage: Option[MoviePageInfo])
  case class MoviePageInfo(moviePage: URL, imdbID: String)
  
  case class SubtitlePageExtractor(url: URL) extends HtmlTask[Option[SubtitlePageInfo]] with Logging {
    import XOM._
    val logID = "SubtitleSource_SubtitlePageExtractor(" +  url.toString + ")"
    
    def process(doc: nu.xom.Document): Option[SubtitlePageInfo] = {
      
      val ctx = Some( XOM.XPath.XHTML )
      
      // all links of content-container
      val allLinks = 
        for (aNode <- doc.xpath("""//xhtml:div[@id="content-container"]//xhtml:a[@href]""", ctx);
             aElem <- aNode.toElement;
             href  <- aElem.attribute("href"))
          yield (aElem, href)
      
      def eq(a: (Element,String), b: (Element,String)): Boolean = a._2 == b._2 // equality on href
      val links = allLinks.distinctNoCount(eq)
      
      val allMoviePages = 
        for ( (aElem, href) <- links;
              m <- Regex.MovieLink.findFirstMatchIn(href) ) yield {

          val imdbID = m.group(1)
          trace("MoviePage=" + href + "; ImdbID=" + imdbID)
          MoviePageInfo(href.toURL, imdbID)
        }
      val moviePages = allMoviePages.distinct
      
      if (moviePages.isEmpty)
        warn("found no MoviePage on SubtitlePage " + url)
      else if (!moviePages.tail.isEmpty)
        warn("found " + moviePages.length + " MoviePages, using only first: " + moviePages.head )
      
      val moviePage = moviePages.headOption
      
      
      val allDownloadUrls = 
        for ( (aElem, href) <- links;
              m <- Regex.DownloadZipLink.findFirstIn(href) ) yield {
          val url = BaseUrl + m
          trace("DownloadURL=" + url)
          url.toURL
        }
      val downloadUrls = allDownloadUrls.distinct     
      
      if (downloadUrls.isEmpty) {
        warn("found no DownloadURL")
        None
      } else {
        if (!downloadUrls.tail.isEmpty)
          warn("found " + downloadUrls.length + " DownloadURLs, using only first: " + downloadUrls.head )
        
        Some( SubtitlePageInfo(url, downloadUrls.head, moviePage) )
      }
    }
  }
}