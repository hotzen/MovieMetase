package moviemetase
package query

object IMDB {
  val CSE = "011282045967305256347:dyc6spozqnc"
  
  val TitleUrlRegex  = """imdb.com/title/tt[0-9]+""".r
  val TitlePathRegex = """/title/tt[0-9]+""".r
  val IdRegex        = """tt[0-9]+""".r
  
  def extractTitleUrls(s: String): List[String] = TitleUrlRegex.findAllIn( s ).map( m => "http://www." + m + "/").toList

  def extractTitlePaths(s: String): List[String] = TitlePathRegex.findAllIn( s ).toList
    
  def extractIds(s: String): List[String] = IdRegex.findAllIn( s ).toList
    
  def extractId(s: String): Option[String] = extractIds(s).headOption
    
  case class ExtractInfos(val id: String) extends HtmlTask[List[MovieInfo]] with Logging {
    val logID = "IMDB.ExtractInfos(" + id + ")"
    
    val info = MovieInfos.IMDB(id)
    def url = info.page
    
    def process(doc: nu.xom.Document): List[MovieInfo] = {
      import Util._
      import XOM._
      import scala.collection.mutable.ListBuffer
      //println(doc.toXML)
      
      val infos = new ListBuffer[MovieInfo]()
      val ctx = XPathContext.XHTML
      
      trace("extracting ...")
      
      // meta-tags
      val metas =
        for (metaNode <- doc.xpath("""//xhtml:head/xhtml:meta""", ctx);
             metaElem <- metaNode.toElement) yield {
          val name  = metaElem.attribute("name").getOrElse( metaElem.attribute("property").getOrElse("") )
          val value = metaElem.attribute("content").getOrElse("")
          (name, value)
        }
            
      // title & release
      var extractTitle = true
      if (extractTitle)
        for ( (name,value) <- metas if name == "og:title") {
          trace("Title/Release from <meta og:title>: " + value)
          infos append MovieInfos.TitleWithRelease(value)
          extractTitle = false
        }
      if (extractTitle)
        for ( (name,value) <- metas if name == "title") {
          trace("Title/Release from <meta title>: " + value)
          infos append MovieInfos.TitleWithRelease(value)
          extractTitle = false
        }


      // description    
      for ( (name,value) <- metas if name == "description") {
        trace("Description from <meta description>: " + value)
        infos append MovieInfos.Description(value)
        extractTitle = false
      }
    
      
      // thumbnails
      var extractThumbs = true
      if (extractThumbs)
        for ( (name,value) <- metas if name == "og:image") {
          trace("Thumbnail from <meta og:image>: " + value)
          infos append MovieInfos.Thumbnail(value.toURL)
          extractThumbs = false
        }
      if (extractThumbs)
        for ( (name,value) <- metas if name == "image_src") {
          trace("Thumbnail from <meta image_src>: " + value)
          infos append MovieInfos.Thumbnail(value.toURL)
          extractThumbs = false
        }
      
      
      // summary
      for (h2Node <- doc.xpath("""//xhtml:h2[text()='Storyline']""", ctx);
           pNode   <- h2Node.xpath("""following-sibling::xhtml:p""", ctx).headOption;
           pElem   <- pNode.toElement) {
        val text = pElem.text.hardTrim
        trace("Summary from <h2>Storyline</h2>: " + text)
        infos append MovieInfos.Summary(text)
      }
      
      // cast table
      for (trNode <- doc.xpath("""//xhtml:table[@class="cast_list"]//xhtml:tr""", ctx);
           trElem <- trNode.toElement) {
        
        val nameOpt = {
          for (tdNode <- trElem.xpath("""xhtml:td[@class="name"]""", ctx);
               tdElem <- tdNode.toElement)
            yield tdElem.value.hardTrim 
        }.headOption
        
        val charOpt = {
          for (tdNode <- trNode.xpath("""xhtml:td[@class="character"]""", ctx);
               tdElem <- tdNode.toElement)
            yield tdElem.value.hardTrim 
        }.headOption
                
        for (name <- nameOpt)
          infos append MovieInfos.Actor(name, charOpt)
      }
      
      // genres
      for (aNode <- doc.xpath("""//xhtml:a""", ctx);
           aElem <- aNode.toElement if aElem.attribute("href").getOrElse("").startsWith("/genre/")) {
        
        val genre = aElem.getValue.noEntities.trim
        infos append MovieInfos.Genre( genre )
      }
      
      infos.toList
    }
  }
  
//    case class GooglePageMapInfos(val id: String) extends Task[List[MovieInfo]] with Logging {
//    val logID = "IMDB.GooglePageMapInfos(" + id + ")"
//    
//    def execute(): List[MovieInfo] = {
//      val info = MovieInfos.IMDB(id)
//      val url = info.page.toString
//
//      trace("Querying Google CSE for URL " + url + " ...")
//      val googleRes = GoogleCSE.Query(CSE, url).execute()
//      
////      for (res <- googleRes if TitleUrlRegex.findFirstIn(res.url.toString).isDefined) {
////        trace("PageMap of " + res.url + ":")
////        for (data <- res.pageMap) {
////          trace(data.toString)
////        }
////      }
//      
//      Nil
//    }
//  }
}
