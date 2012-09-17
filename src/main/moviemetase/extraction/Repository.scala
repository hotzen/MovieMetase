package moviemetase
package extraction

import java.net.URL

object Repository extends Logging {
  val logID = "ExtractorRepository"
  
  type EntryID = String
  val indexID = "L0Z7yLkp"
  
  def all(): List[Extractor[_]] =
    PasteBin.raw(indexID).toList.
      flatMap( indexRaw => parseIndex(indexID, indexRaw) ).
      flatMap( entryID => get(entryID) )

  def get(id: EntryID): Option[Extractor[_]] =
    PasteBin.raw(id).
      flatMap( parseExtractor(id, _) )

  def parseIndex(id: EntryID, raw: String): List[EntryID] = 
    raw.split("\n").
      map(_.trim()).
      filter(x => !x.isEmpty && !x.startsWith("#")).
      toList match {
        case Nil => {
          error("Index " + PasteBin.url(id) + " does not contain any entries")
          Nil
        }
        case x => x
      }

  def parseExtractor(id: EntryID, raw: String): Option[Extractor[_]] =
    try DSL(raw).headOption
    catch { case e:InvalidExtractor => {
      error("Entry " + PasteBin.url(id) + " specifies an invalid extractor: " + e.msg)
      None
    }}
}

object PasteBin {
  def url(id: String): URL =
    new URL("http://pastebin.com/raw.php?i=" + id)
    
  def raw(id: String): Option[String] = {
    // TODO
    None
  }
}