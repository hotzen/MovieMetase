package moviemetase
package extraction

import Util._

class SubtitleFactory extends Factory[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleFactory"
  
  def create(extracts: List[(String, String)]): List[MovieInfos.Subtitle] = {
    if (tracing)
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