package moviemetase
package scraping

import Util._

class SubtitleFactory(scraper: Scraper[MovieInfos.Subtitle]) extends Factory[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleFactory"
  
  def create(extracts: List[(String, String)]): List[MovieInfos.Subtitle] = {
    if (scraper.tracing)
      trace("extracts: "+ extracts.mkString(", "))

    val extractsLC = extracts.map({case (n,v) => (n.toLowerCase, v) })
          
    val label = extractsLC.collect({ case ("subtitle-label",v) => v }).headOption.getOrElse("N/A")
    val lang = extractsLC.collect({ case ("subtitle-langtext",v) => v }).headOption.getOrElse("N/A")
    val page = extractsLC.collect({ case ("subtitle-pageurl",v) => v }).headOption.getOrElse("http://unknown.net").toURL
    val dl = extractsLC.collect({ case ("subtitle-downloadurl",v) => v }).headOption.map(_.toURL)
    val rel = extractsLC.collect({ case ("subtitle-releasetext",v) => v }).headOption
    
    MovieInfos.Subtitle(label, lang, page, dl, rel) :: Nil
  }
}

case class SubtitleScraper(desc: String, start: Step[MovieInfos.Subtitle]) extends PageScraper[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleScraper(" + desc + ")"
  
  val factory = new SubtitleFactory(this)
} 

case class SubtitleSearcher(desc: String, start: Step[MovieInfos.Subtitle]) extends SearchScraper[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleSearcher(" + desc + ")"

  val factory = new SubtitleFactory(this)
}
