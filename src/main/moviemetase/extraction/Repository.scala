package moviemetase
package extraction

import scala.concurrent.Future
import java.net.URL

object Repository extends Logging {
  import TaskManager.async._
  val logID = "ExtractorRepository"
  
  val defaultID = "L0Z7yLkp"
  def load(): Future[List[Extractor[_]]] = load(defaultID)
      
  def load(id: String): Future[List[Extractor[_]]] =
    getPasteBinRaw(id).map( raw => parse(raw, id) )

  def parse(raw: String, id: String): List[Extractor[_]] =
    try DSL(raw)
    catch { case e:InvalidExtractor => {
      error(id + " specifies an invalid extractor:\n" + e.msg)
      Nil
    }}

  def getPasteBinRaw(id: String): Future[String] = {
    info("loading extractors from PasteBin/" + id)
    HttpTask.GET( new URL("http://pastebin.com/raw.php?i=" + id) )
  }
}