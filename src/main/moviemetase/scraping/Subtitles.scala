package moviemetase
package scraping

import Util._

case class SubtitleScraper(desc: String, start: Step[MovieInfos.Subtitle]) extends Scraper[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleScraper(" + desc + ")"
    
  val factory = new Factory[MovieInfos.Subtitle] {
    def create(extracts: List[(String, String)]): List[MovieInfos.Subtitle] = {
      //trace("extracts: "+ extracts.mkString(",\n  "))
      val extractsLC = extracts.map({case (n,v) => (n.toLowerCase, v) })
      
      val label = extractsLC.collect({ case ("subtitle-label",v) => v }).headOption.getOrElse("N/A")
      val lang = extractsLC.collect({ case ("subtitle-langtext",v) => v }).headOption.getOrElse("N/A")
      val page = extractsLC.collect({ case ("subtitle-pageurl",v) => v }).headOption.getOrElse("http://unknown.net").toURL
      val dl = extractsLC.collect({ case ("subtitle-downloadurl",v) => v }).headOption.getOrElse("http://unknown.net").toURL
      
      MovieInfos.Subtitle(label, lang, page, dl) :: Nil
    }
  }
}
