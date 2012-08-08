package moviemetase
package scraping

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

case class Context[A](results: List[A], idents: Map[String, String]) {
  def add(result: A): Context[A] = Context(result :: results, idents)
  
  def ident(id: String): String = idents.get(id) match {
    case Some(v) => v
    case None => throw new Exception("Invalid Identifier '" + id + "'")
  }
}

trait Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A]
}

case class TerminalStep[A]() extends Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A] = ctx.results
}

case class BrowseStep[A](expr: Expr, next: Step[A]) extends Step[A] with Logging {
  val logID = "Browse"
  
  def browse(theUrl: URL): Document = new HtmlTask[Document] {
    def url = theUrl
    def processDocument(doc: Document): Document = doc
  }.execute()

  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    val url = new URL(value)
    trace("browsing '" + url + "'")
    val doc = browse(url)
    next.process(doc.body, ctx)
  }
}

case class SelectStep[A](selector: String, next: Step[A]) extends Step[A] with Logging {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
 
  val logID = "Select"
    
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val selElems = elem.select(selector)
    trace("selected '" + selector + "': " + selElems.size)
    
    selElems.iterator.flatMap(selElem => next.process(selElem, ctx)).toList
  }
}

trait ExtractorFactory[A] {
  def create(what: String, value: String): A
}

case class ExtractStep[A](what: String, expr: Expr, factory: ExtractorFactory[A], next: Step[A]) extends Step[A] with Logging {
  val logID = "Extract(" + what + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    val result = factory.create(what, value)
    trace("extracted '" + value + "': " + result)

    next.process(elem, ctx add result)
  }
}

case class Scraper[A](site: String, author: String, start: Step[A]) extends Logging {
  val logID = "Scrape(" + site + ")"
  
  def scrape(query: String): List[A] = {
    val html = """<html><head></head><body></body></html>"""
    val doc = org.jsoup.Jsoup.parse(html)
    
    val idents = Map[String, String](
      "QUERY" -> query
    )
    trace("identifiers: " + idents.mkString)

    val ctx = Context[A](Nil, idents)
    
    start.process(doc.body, ctx)
  }
}

sealed trait Expr
  case class LiteralExpr(s: String) extends Expr
  case class SelectExpr(selector: String) extends Expr
  case class AttributeExpr(name: String) extends Expr
  case class SelectAttributeExpr(selector: String, name: String) extends Expr
  case class ConcatExpr(a: Expr, b: Expr) extends Expr
  case class IdentExpr(n: String) extends Expr // e.g. "SEARCH-TERM", better name?!
  // TODO RegexExpr
  // ...

object Expr {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
  
  def apply(e: Expr, elem: Element, ctx: Context[_]): String = e match {
    case LiteralExpr(s) => s
    case SelectExpr(s) => {
      val elems = elem.select(s)
      if (elems.size == 0) ""
      else elems.get(0).text
    }
    case AttributeExpr(a) =>
      elem.attr(a)

    case SelectAttributeExpr(s, a) => {
      val elems = elem.select(s)
      if (elems.size == 0) ""
      else elems.get(0).attr(a)
    }
    case ConcatExpr(a, b) =>
      apply(a, elem, ctx) + apply(b, elem, ctx)
    case IdentExpr(id) =>
      ctx.ident(id)
  }
}
  
//SEARCH SUBTITLE AT "SubtitleSource.org"
//  BROWSE "http://www.subtitlesource.org/search/" + SEARCH-TERM
//  SELECT "#searchPage li a"
//  BROWSE "http://www.subtitlesource.org/" + ATTRIBUTE "href"
//  SELECT "ul#subtitle-list li"
//  
//  EXTRACT SUBTITLE-DOWNLOAD FROM "a:eq(1)" ATTRIBUTE "href" 
//  EXTRACT SUBTITLE-PAGE FROM "a:eq(2)" ATTRIBUTE "href" 
//  EXTRACT SUBTITLE-LANGUAGE FROM "a:eq(2)" ATTRIBUTE "title" 
//ENDSEARCH

object DSL extends StandardTokenParsers {
  def parse = null
  
  def apply(s: String): Unit = {
    val tokens = new lexical.Scanner(s)
    phrase(parse)(tokens)
  }
}


object DSL_Test {
  def main(args: Array[String]): Unit = {
    
//    Search("SubtitleSource.org",
//        Select("#searchPage li a[href]",
//            Browse(
//              Terminal
//            )
//          )
//        ).search("foo")
//      
//      Thread.sleep(100000)
    
//    DSL("""
//SEARCH SUBTITLES AT "SubtitleSource.org"
//  GET "http://www.subtitlesource.org/search/$$$"
//
//  SELECT "#searchPage li"
//  SELECT "a[href]"
//  GET SELECTED
//  
//  SELECT "table#release-list tr td#subtitle-container"
//   
//  SELECT "ul#subtitle-list li"
//  
//  SELECT "a:eq(1)[href]"  AS SUBTITLE-DOWNLOAD
//  SELECT "a:eq(2)[href]"  AS SUBTITLE-PAGE
//  SELECT "a:eq(2)[title]" AS SUBTITLE-LANGUAGE
//ENDSEARCH""")
  
  }
}