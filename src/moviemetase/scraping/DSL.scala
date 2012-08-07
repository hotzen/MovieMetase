package moviemetase
package scraping

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

object DSL extends StandardTokenParsers {
  
  def parse = null
  
  def apply(s: String): Unit = {
    val tokens = new lexical.Scanner(s)
    phrase(parse)(tokens)
  }
}


trait Action {
  def process(elem: Element): List[Element]
  def next: Action
}

case class GetAction(url: URL, next: Action) extends Action {
  val task = new HtmlTask[Document] {
    def url = new URL( url.toExternalForm.replace("$$$", "foo") )
    def processDocument(doc: Document): Document = doc
  }
  
  def process(elem: Element): List[Element] = {
    val doc = task.execute()
    doc.body :: Nil
  }
}

case class SelectAction(selector: String, next: Action) extends Action {
  def process(elem: Element): List[Element] = {
    val elems = elem.select(selector)
    val iter = elems.iterator()
    scala.collection.convert.Wrappers.JIteratorWrapper[Element]( iter ).toList
  }
}


case class SubtitleSearch(siteName: String, action: Action) {
  
  def search(term: String): List[MovieInfos.Subtitle] = {
    
    Nil
  }
}


object DSL_Test {
  def main(args: Array[String]): Unit = {
    DSL("""
SEARCH SUBTITLES AT "SubtitleSource.org"
  GET "http://www.subtitlesource.org/search/$$$"

  SELECT "#searchPage li"
  SELECT "a[href]"
  GET SELECTED
  
  SELECT "table#release-list tr td#subtitle-container"
   
  SELECT "ul#subtitle-list li"
  
  SELECT "a:eq(1)[href]"  AS SUBTITLE-DOWNLOAD
  SELECT "a:eq(2)[href]"  AS SUBTITLE-PAGE
  SELECT "a:eq(2)[title]" AS SUBTITLE-LANGUAGE
ENDSEARCH""")
  
  }
}