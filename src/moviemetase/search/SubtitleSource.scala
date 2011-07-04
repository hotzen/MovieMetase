package moviemetase
package search

import Util._
import java.net.URL
import java.io.InputStream
import nu.xom._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import java.util.concurrent.Future

sealed trait SubtitleSourceQuery extends Query[Movie]

object SubtitleSource {
  
  val BaseUrl = "http://www.subtitlesource.org"
  
  val ReleaseCSE = "011282045967305256347:2mp-i5b1ixi"
  
  object Regex {
    val MovieLink       = """/title/(tt\d+)""".r // yay, an IMDB-ID!
    val ReleaseLink     = """/release/(\d+)[^"]*""".r
    val SubtitleLink    = """/subs/(\d+)[^"]*""".r  
    val DownloadZipLink = """/download/zip/(\d+)""".r
  }
    
  case class ReleaseSearch(val id: String) extends Search[List[MovieInfo]] with Logging {
    val logID = "SubtitleSource_ReleaseSearch(" + id  + ")"
    
    val BaseScore = 0.95
    
    def search(term: String): List[(Double,List[MovieInfo])] = {
      val fuzzy = Google.fuzzyTerm(term)
      trace("querying GoogleCSE with term '" + fuzzy + "'")
      
      val relQuery = GoogleCSE.Query(ReleaseCSE, term)
      val fut = relQuery.execute()
      val res = fut.get() // block
      
      // extract Release-Page
      val scoredReleaseInfosFuts =
        for ( (r, idx) <- res.zipWithIndex;
               _       <- Regex.ReleaseLink.findFirstIn(r.url.toString) ) yield {

          val score = BaseScore * (1.0 - idx * 0.05)
          trace("ResultIndex=" + idx + "; ReleasePage=" + r.url + "; Score=" + score)
          
          val extract = ReleasePageExtractor( r.url )
          val fut = extract.execute()
          (score, fut)
        }
      
      // join Futures
      val allScoredReleaseInfos =
         for ( (score, fut) <- scoredReleaseInfosFuts;
                link        <- fut.get() ) yield (score, link)      

      // filter out duplicate Subtitle-Pages
      def eq(a: (Double, ReleasePageInfo), b: (Double, ReleasePageInfo)): Boolean =
        a._2.subtitlePage == b._2.subtitlePage
      val scoredReleaseInfos = allScoredReleaseInfos.countedDistinct(eq).sortByCount().noCount()
            
      // extract Subtitle-page
      val scoredInfosFuts =
        for ( (score, releaseInfo) <- scoredReleaseInfos ) yield {
          val extract = SubtitlePageExtractor( releaseInfo.subtitlePage )
          val fut = extract.execute()
          (score, releaseInfo, fut)
        }
      
      // join Futures
      val scoredInfos =
        for ( (score, releaseInfo, fut) <- scoredInfosFuts) yield 
          (score, releaseInfo, fut.get())
      
      // generate MovieInfo
      val infos =
        for ( (score,releaseInfo,subInfoOpt) <- scoredInfos;
              subInfo <- subInfoOpt) yield {
        val movieInfos = new ListBuffer[MovieInfo]()
        // ReleasePageInfo(releasePage: URL, subtitlePage: URL, label: String, lang: String)
        // SubtitlePageInfo(subtitlePage: URL, downloadUrl: URL, moviePage: Option[MoviePageInfo])
        // MoviePageInfo(moviePage: URL, imdbID: String)

        movieInfos append MovieInfos.Subtitle(
          releaseInfo.label,
          releaseInfo.lang,
          subInfo.downloadUrl
        ).withSourceInfo( subInfo.subtitlePage.toString )
        
        if (!subInfo.moviePage.isEmpty) {
          val moviePage = subInfo.moviePage.get
          movieInfos append MovieInfos.IMDB(
            moviePage.imdbID
          ).withSourceInfo( moviePage.moviePage.toString )
        }
        
        (score, movieInfos.toList)
      }
      
      infos.toList
    }
  }
  
  case class ReleasePageInfo(releasePage: URL, subtitlePage: URL, label: String, lang: String)
  
  case class ReleasePageExtractor(url: URL) extends HtmlProcessor[List[ReleasePageInfo]] with Logging {
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
  
  case class SubtitlePageExtractor(url: URL) extends HtmlProcessor[Option[SubtitlePageInfo]] with Logging {
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
      val links = allLinks.countedDistinct(eq).noCount()
      
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
        warn("found " + moviePages.length + " MoviePages, using only first: " + moviePages.first)
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
          warn("found " + downloadUrls.length + " DownloadURLs, using only first: " + downloadUrls.first)
        
        Some( SubtitlePageInfo(url, downloadUrls.head, moviePage) )
      }
    }
  }
}