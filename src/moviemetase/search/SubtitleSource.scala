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
      val scoredReleaseInfos =
         for ( (score, fut) <- scoredReleaseInfosFuts;
                link        <- fut.get() ) yield {
           
           trace("Subtitle-Page=" + link + "; Score=" + score)
           (score, link)
         }
      

      // filter out duplicate Subtitle
      def eq(a: (Double, ReleasePageInfo), b: (Double, ReleasePageInfo)): Boolean =
        a._2.subtitlePage == b._2.subtitlePage
      val scoredReleaseInfos2 = scoredReleaseInfos.packed(eq).sortWith( (a,b) => a._2 > b._2 ).map( p => p._1 )
      
      
      // extract Subtitle-page
      val scoredSubtitleInfosFuts =
        for ( (score, releaseInfo) <- scoredReleaseInfos2 ) yield {
          val extract = SubtitlePageExtractor( releaseInfo.subtitlePage )
          val fut = extract.execute()
          (score, fut)
        }
      
      // join Futures of Movie-Infos
      val scoredSubtitleInfos =
        for ( (score, fut) <- scoredSubtitleInfosFuts) yield 
          (score, fut.get())
      
      scoredSubtitleInfos
    }
  }
  
  case class ReleasePageInfo(releasePage: URL, subtitlePage: URL, label: String, lang: String)
  
  case class ReleasePageExtractor(url: URL) extends HtmlProcessor[List[ReleasePageInfo]] with Logging {
    import XOM._
    val logID = "SubtitleSource_ReleasePageExtractor(" +  url.toString + ")"

    def process(doc: nu.xom.Document): List[ReleasePageInfo] = {
      trace("extracting...")
      
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
  
  case class SubtitlePageExtractor(url: URL) extends HtmlProcessor[List[MovieInfo]] with Logging {
    import XOM._
    val logID = "SubtitleSource_SubtitlePageExtractor(" +  url.toString + ")"
    
    def process(doc: nu.xom.Document): List[MovieInfo] = {
      trace("extracting...")
      
      val infos = new ListBuffer[MovieInfo]
      val ctx = Some( XOM.XPath.XHTML )
      
      var downloadLink: Option[String] = None
      
      // all links
      for (aNode <- doc.xpath("""//xhtml:a[@href]""", ctx);
           aElem <- aNode.toElement;
           href  <- aElem.attribute("href")) {
        
        // link to SubtitleSource's Movie-Page. Contains the IMDB-ID, yay!
        for (m <- Regex.MovieLink.findFirstMatchIn(href) ) {
          val imdbID = m.group(1)
          trace("MoviePage=" + href + "; ImdbID=" + imdbID)          
          infos append MovieInfos.IMDB( imdbID )
        }
        
        // link to the ZIP-Download
        for (m <- Regex.DownloadZipLink.findFirstIn(href) ) {
          val url  = BaseUrl + m
          trace("DownloadURL=" + url)
          downloadLink = Some( url )
        }
      }
      
      // information-box containing the download-link (which we are not interested in, since it already has been extracted above)
      // and the label and language of the subtitle
//      for (boxNode <- doc.xpath("""//xhtml:div[@id="subtitle-information-box"]""", ctx);
//           boxElem <- boxNode.toElement) {
//        
//        // no good heuristic to extract language on this page. it is only encoded in the alt/title attribute of the image
//        // and the image-path itself is not unique either :/
//        //val lang = ... 
//        
//        val label =
//          for (h3Node <- boxElem.xpath("""//xhtml:h3""", ctx);
//               aNode  <- h3Node.xpath("""//xhtml:a""", ctx);
//               aElem  <- aNode.toElement)
//            yield aElem.getValue
//            
//        println( contentElem.toXML )
//        println("---\n\n")
//      }
      
      infos.toList
    }
  }
}