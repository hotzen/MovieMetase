package moviemetase
package scraping

import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import scala.collection._

// context while processing an element inside a step 
case class Context[A](baseURL: URL, extracts: List[(String, String)], factory: Factory[A],
                      params: Map[String, String], vars: Map[String, String]) {
  
  def setBaseURL(url: URL): Context[A] =
    Context(url, extracts, factory, params, vars)
  
  def addExtract(name: String, value: String): Context[A] =
    Context(baseURL, (name, value) :: extracts, factory, params, vars)
  
  def setParam(n: String, v: String): Context[A] =
    Context(baseURL, extracts, factory, params + (n -> v), vars)
    
  def setVar(n: String, v: String): Context[A] =
    Context(baseURL, extracts, factory, params, vars + (n -> v))
}

trait Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A]
}

trait TracingStep {
  var tracing = false
  
  def trace(on: Boolean): this.type = {
    tracing = on
    this
  }
}

case class FinalStep[A]() extends Step[A] with Logging {
  val logID = "Final"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val results = ctx.factory.create( ctx.extracts )
    if (results.isEmpty)
      warn("no results")
    
    results.reverse
  }
  
  override def toString = "End"
}

case class BrowseStep[A](expr: Expr, next: Step[A], base: Option[String]) extends Step[A] with TracingStep with Logging {
  val logID = "Browse"
  
  def browse(theUrl: URL): Document = new HtmlTask[Document] {
    def url = theUrl
    def processDocument(doc: Document): Document = doc
  }.execute()

  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    val url = new URL(value)
    
    if (tracing)
      trace(url.toExternalForm)
      
    val baseURL = base match {
      case Some(b) => new URL(b)
      case None => url
    }
      
    val doc = browse(url)
    next.process(doc.body, ctx setBaseURL baseURL)
  }
  
  override def toString = "Browse(" + expr + ")\n" + next.toString
}

case class SelectStep[A](selector: String, max: Int, next: Step[A]) extends Step[A] with TracingStep with Logging {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
 
  val logID = "Select(" + selector + { if (max < Int.MaxValue) ", Max="+max else "" } + ")"
    
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val selElems = elem.select(selector)
    
    if (selElems.isEmpty)
      warn("no match!")
    else if (tracing)
      trace("selected " + selElems.size)
      
    selElems.iterator.take(max).flatMap(selElem => {
      next.process(selElem, ctx)
    }).toList
  }
  
  override def toString = "Select("+selector+")\n" + next.toString
}

trait Factory[A] {
  def create(extracts: List[(String,String)]): List[A]
}

case class ExtractStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with TracingStep with Logging {
  val logID = "Extract(" + name + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    if (tracing)
      trace("extracted " + name + ": '" + value + "'")
    next.process(elem, ctx.addExtract(name, value))
  }
    
  override def toString = "Extract(" + name + ", " + expr + ")\n" + next.toString
}

case class StoreStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with TracingStep with Logging {
  val logID = "Store(" + name + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    if (tracing)
      trace("'" + value + "'")

    next.process(elem, ctx setVar (name, value))
  }
    
  override def toString = "Store(" + name + ", " + expr + ")\n" + next.toString
}

trait Scraper[A] {
  def start: Step[A]
  def factory: Factory[A]
  
  def scrape(query: String): List[A] = {
    val baseURL = new URL("http://initial.net/")
    val html = """<html><head></head><body></body></html>"""
    val doc = org.jsoup.Jsoup.parse(html)
    val elem = doc.body
    
    val args = Map[String, String](
      "QUERY" -> query
    )
    val vars = Map[String, String]()
    val ctx = Context[A](baseURL, Nil, factory, args, vars)
    
    start.process(elem, ctx)
  }
  
  override def toString = "Scraper: " + start.toString
}


sealed trait Expr
  case class LitExpr(s: String) extends Expr
  case class ParamExpr(name: String) extends Expr
  case class VarExpr(name: String) extends Expr
  case class SelExpr(selector: String) extends Expr
  case class AttrExpr(name: String) extends Expr
  case class SelAttrExpr(selector: String, name: String) extends Expr
  case class UrlExpr(e: Expr) extends Expr
  case class ConcatExpr(a: Expr, b: Expr) extends Expr
  trait SubstrExpr extends Expr {
    def expr: Expr
    def absPos(pos: Int, len: Int): Int = if (pos < 0) len + pos else pos
    def apply(s: String): String
  }
  case class SubstrEndExpr(expr: Expr, off: Int) extends SubstrExpr {
    def apply(s: String): String =
      s.substring(absPos(off, s.length))
  }
  case class SubstrLenExpr(expr: Expr, off: Int, len: Int) extends SubstrExpr {
    def apply(s: String): String = {
      val a = absPos(off, s.length)
      val b = a + len
      s.substring(a, b)
    }
  }
  case class SubstrPosExpr(expr: Expr, off: Int, pos: Int) extends SubstrExpr {
    def apply(s: String): String = {
      val len = s.length
      val a = absPos(off, len)
      val b = absPos(pos, len)
      s.substring(a, b)
    }
  }
  case class RegexExpr(expr: Expr, pattern: String) extends Expr {
    val regex = pattern.r
    // TODO
//    def apply(s: String): List[(Int,String)] = {
//      { for (m <- regex.findAllMatchIn(s)) yield {
//        
//        (0,"")
//      }}.toList       
//    }
  }

object Expr {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
      
  def apply(e: Expr, elem: Element, ctx: Context[_]): String = e match {
    case LitExpr(s) => s
    case ParamExpr(name) => ctx.params.get(name) match {
      case Some(v) => v
      case None => throw new Exception("invalid parameter '" + name + "'")
    }
    case VarExpr(name) => ctx.vars.get(name) match {
      case Some(v) => v
      case None => throw new Exception("invalid variable '" + name + "'")
    }  
    case SelExpr(sel) => {
      val elems = elem.select(sel)
      if (elems.size == 0) ""
      else elems.get(0).text
    }
    case AttrExpr(attr) =>
      elem.attr(attr)
    case SelAttrExpr(sel, attr) => {
      val elems = elem.select(sel)
      if (elems.size == 0) ""
      else elems.get(0).attr(attr)
    }
    case UrlExpr(e) => {
      val s = apply(e, elem, ctx)
      new URL(ctx.baseURL, s).toExternalForm
    }
    case ConcatExpr(a, b) => {
      val sb = new StringBuilder
      sb append apply(a, elem, ctx)
      sb append apply(b, elem, ctx)
      sb.toString
    }
    case substr:SubstrExpr =>
      substr.apply( apply(substr.expr, elem, ctx) )
    case RegexExpr(e, p) =>
      throw new UnsupportedOperationException("RegexExpr TODO")
  }
}