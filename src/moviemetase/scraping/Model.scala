package moviemetase
package scraping

import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

case class Context[A](results: List[A], factory: ExtractorFactory[A], vars: Map[String, String]) {
  
  def add(result: A): Context[A] =
    Context(result :: results, factory, vars)
  
  def value(v: String): String = vars.get(v) match {
    case Some(v) => v
    case None => throw new Exception("Invalid Variable '" + v + "'")
  }
}

trait Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A]
}

case class TerminalStep[A]() extends Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A] = ctx.results
  
  override def toString = "End"
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
  
  override def toString = "Browse(" + expr + ")\n" + next.toString
}

case class SelectStep[A](selector: String, max: Int, next: Step[A]) extends Step[A] with Logging {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
 
  val logID = "Select"
    
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val selElems = elem.select(selector)
    trace("selected '" + selector + "': " + selElems.size)
    
    selElems.iterator.take(max).flatMap(selElem => next.process(selElem, ctx)).toList
  }
  
  override def toString = "Select("+selector+")\n" + next.toString
}

trait ExtractorFactory[A] {
  def create(what: String, value: String): A
}

case class ExtractStep[A](what: String, expr: Expr, next: Step[A]) extends Step[A] with Logging {
  val logID = "Extract(" + what + ")"
  
  val factory: ExtractorFactory[A] = null
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    val result = factory.create(what, value)
    trace("extracted '" + value + "': " + result)

    next.process(elem, ctx.add(result))
  }
  
  override def toString = "Extract(" + what + " <= " + expr + ")\n" + next.toString
}

trait Scraper[A] {
  def start: Step[A]
  def factory: ExtractorFactory[A]
  
  def scrape(query: String): List[A] = {
    val html = """<html><head></head><body></body></html>"""
    val doc = org.jsoup.Jsoup.parse(html)
    val elem = doc.body
    
    val idents = Map[String, String](
      "QUERY" -> query
    )
    
    val ctx = Context[A](Nil, factory, idents)
    start.process(elem, ctx)
  }
  
  override def toString = "Scraper: " + start.toString
}

case class SubtitleScraper(site: String, author: String, start: Step[MovieInfos.Subtitle]) extends Scraper[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleScraper(" + site + ")"
  val factory = new ExtractorFactory[MovieInfos.Subtitle] {
    def create(what: String, value: String): MovieInfos.Subtitle = {
      MovieInfos.Subtitle("test", "de", new URL("http://foo.bar"), new URL("http://foo.baz"))
    }
  }
}

sealed trait Expr
  case class LitExpr(s: String) extends Expr
  case class VarExpr(n: String) extends Expr
  case class SelExpr(selector: String) extends Expr
  case class AttrExpr(name: String) extends Expr
  case class SelAttrExpr(selector: String, name: String) extends Expr
  case class ConcatExpr(a: Expr, b: Expr) extends Expr
  case class SubstrLenExpr(e: Expr, off: Int, len: Int) extends Expr
  case class SubstrPosExpr(e: Expr, off: Int, pos: Int) extends Expr
  // TODO RegexExpr
  // ...

object Expr {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
  
  def apply(e: Expr, elem: Element, ctx: Context[_]): String = e match {
    case LitExpr(s) => s
    case VarExpr(v) => ctx value v
    case SelExpr(s) => {
      val elems = elem.select(s)
      if (elems.size == 0) ""
      else elems.get(0).text
    }
    case AttrExpr(a) =>
      elem.attr(a)
    case SelAttrExpr(s, a) => {
      val elems = elem.select(s)
      if (elems.size == 0) ""
      else elems.get(0).attr(a)
    }
    case ConcatExpr(a, b) => {
      val sb = new StringBuilder
      sb append apply(a, elem, ctx)
      sb append apply(b, elem, ctx)
      sb.toString
    }
    case SubstrLenExpr(e, off, len) => {
      val s = apply(e, elem, ctx)
      s.substring(off, off+len)
    }
    case SubstrPosExpr(e, off, pos) => {
      val s = apply(e, elem, ctx)
      s.substring(off, pos)
    }
  }
}