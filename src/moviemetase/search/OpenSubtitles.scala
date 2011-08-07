package moviemetase
package search

import java.net.URL
import nu.xom._
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.json.JSON
import Util._
import java.io.OutputStream
import java.io.PrintWriter

//case class OpenSubtitlesResult(url: URL, title: String, snippet: String, query: OpenSubtitlesQuery) {
//  def toSubtitle: MovieInfos.Subtitle = null
//}
//
//sealed trait OpenSubtitlesQuery extends Query[OpenSubtitlesResult] with XmlProcessor[List[OpenSubtitlesResult]] {
//  final override val UserAgent = OpenSubtitles.API_UA
//  final override val RequestContentType = Some("text/xml")
//  
//  final def url: URL = new URL( OpenSubtitles.API_URL )
//}
//
// http://trac.opensubtitles.org/projects/opensubtitles/wiki/XMLRPC
object OpenSubtitles {

  val API_UA  = "MovieMetase v1"
  val API_URL = "http://api.opensubtitles.org/xml-rpc"
  
  var defaultLanguage: String = "eng"
  
  // http://trac.opensubtitles.org/projects/opensubtitles/wiki/XmlRpcLogIn
  case class Login(user: String = "", password: String = "", language: String = defaultLanguage) extends XmlProcessor[Option[String]] {
    override val UserAgent = API_UA
    override val RequestContentType = Some("text/xml")
    
    override val RequestFn = Some( (os:OutputStream) => {
      val pw  = new PrintWriter( os )
      val rpc = XmlRpc.methodCall("LogIn", user:: password :: language :: API_UA :: Nil)
      
      pw.write( rpc )
      pw.flush()
      os.close()
    })
    
    def url: URL = new URL( API_URL )
    
    def process(doc: nu.xom.Document): Option[String] = {
      import XOM._
      
      val tokens =
        for (node  <- doc.xpath("""//member[name="token"]/value/string""");
             token <- node.toElement) yield token.getValue

      tokens.headOption
    }
  }
  
  // http://trac.opensubtitles.org/projects/opensubtitles/wiki/XmlRpcSearchSubtitles
  case class SearchByImdb(token: String, imdb: MovieInfos.IMDB, language: String = defaultLanguage) extends XmlProcessor[List[MovieInfos.Subtitle]] {
    override val UserAgent = OpenSubtitles.API_UA
    override val RequestContentType = Some("text/xml")
  
    def url: URL = new URL( OpenSubtitles.API_URL )
    
    override val RequestFn = Some( (os:OutputStream) => {
      val pw  = new PrintWriter( os )

      val imdbID = imdb.id.drop(2) // skip leading "tt" of ID
      val searchStruct = ("sublanguageid" -> language) :: ("imdbid" -> imdbID) :: Nil
      val searches = Array( searchStruct )
      val rpc = XmlRpc.methodCall("SearchSubtitles", token :: searches :: Nil)
      
      pw.write( rpc )
      pw.flush()
      os.close()
    })
    
    def process(doc: nu.xom.Document): List[MovieInfos.Subtitle] = {
      import XOM._
      
      for (structNode <- doc.xpath("""//member[name="data"]//struct""");
           structElem <- structNode.toElement) yield {

        def memberValue(name: String): Option[String] = {
          val values =
            for (node <- structElem.xpath("""member[name="""" + name + """"]/value/*""");
                 elem <- node.toElement) yield elem.getValue
          values.headOption
        }
          
        val labelParts = memberValue("MovieName") :: memberValue("MovieReleaseName") :: memberValue("SubFileName") :: memberValue("SubAuthorComment") :: Nil
        val label = labelParts.flatten.map(_.trim.noTags.noEntities).filter(_.length > 0).mkString(" / ")
        
        val lang  = memberValue("SubLanguageID").get
        
        val subID = memberValue("IDSubtitle").get
        val page = ("http://www.opensubtitles.org/en/subtitles/" + subID + "/").toURL
        
        val file  = memberValue("ZipDownloadLink").get.toURL
        
        MovieInfos.Subtitle(label, lang, page, file)
      }
    }
  }
}


