package moviemetase
package scraping

import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import scala.collection._

// context while processing an element inside a step 
case class Context[A](baseURL: URL, results: List[A], factory: ExtractorFactory[A],
                      params: Map[String, String], vars: Map[String, String]) {
  
  def withBaseURL(url: URL): Context[A] =
    Context(url, results, factory, params, vars)
  
  def withResult(result: A): Context[A] =
    Context(baseURL, result :: results, factory, params, vars)
  
  def withParam(n: String, v: String): Context[A] =
    Context(baseURL, results, factory, params + (n -> v), vars)
    
  def withVar(n: String, v: String): Context[A] =
    Context(baseURL, results, factory, params, vars + (n -> v))
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

case class TerminalStep[A]() extends Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A] = ctx.results
  
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
      trace(url.toString)
      
    val baseURL = base match {
      case Some(b) => new URL(b)
      case None => url
    }
      
    val doc = browse(url)
    next.process(doc.body, ctx withBaseURL baseURL)
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

trait ExtractorFactory[A] {
  def create(what: String, value: String): A
}

case class ExtractStep[A](what: String, expr: Expr, next: Step[A]) extends Step[A] with TracingStep with Logging {
  val logID = "Extract(" + what + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    val result = ctx.factory.create(what, value)
    
    if (tracing)
      trace("extracted: '" + value + "', crafted: " + result)
    
    next.process(elem, ctx withResult result)
  }
    
  override def toString = "Extract(" + what + " <= " + expr + ")\n" + next.toString
}

case class StoreStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with TracingStep with Logging {
  val logID = "Store(" + name + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = Expr.apply(expr, elem, ctx)
    
    if (tracing)
      trace(" = '" + value + "'")

    next.process(elem, ctx withVar (name, value))
  }
    
  override def toString = "Store(" + name + " <= " + expr + ")\n" + next.toString
}

object PropertiesStore {
  type PropertyClass = String
  type PropertyName  = String
  type PropertyValue = String
  
  type MapType = mutable.Map[PropertyClass,mutable.Map[PropertyName,PropertyValue]]
  val empty: MapType = mutable.Map[PropertyClass,mutable.Map[PropertyName,PropertyValue]]()
  
  private val parseRegex = """([a-zA-Z0-9_]+)\-([a-zA-Z0-9_]+)""".r
  
  // String => Some(PropClazz, PropName)
  def parseKeys(s: String): Option[(String, String)] = parseRegex.findPrefixMatchOf(s) match {
    case Some(m) => {
      val clazz = m.group(1)
      val name = m.group(2)
      Some((clazz, name))
    }
    case None => None
  }
}

trait PropertiesStore {
  import PropertiesStore._
  
  val store: MapType = empty
  
  def put(clazz: String, name: String, value: String): this.type = { 
    store get clazz match {
      case Some(props) =>
        props put (name, value)
      case None => {
        val props = mutable.Map[String,String]()
        props put (name, value)
        store.put(clazz, props)
      }
    }
    this
  }
}

trait Scraper[A] {
  def start: Step[A]
  def factory: ExtractorFactory[A]
  
  def scrape(query: String): List[A] = {
    val baseURL = new URL("http://UNKNOWN.net/")
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

case class SubtitleScraper(desc: String, start: Step[MovieInfos.Subtitle]) extends Scraper[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleScraper(" + desc + ")"
  val factory = new ExtractorFactory[MovieInfos.Subtitle] with PropertiesStore {
    import PropertiesStore._
    
    def create(what: String, value: String): MovieInfos.Subtitle = {
      parseKeys(what) match {
        case Some((clazz, name)) => {
          trace(clazz+"/"+name + " = '" + value + "'")
          put(clazz, name, value)
        }
        case None => warn("invalid clazz/name term '" + what + "'")
      }
      
      val label = what + ": " + value
      val lang = "" 
      MovieInfos.Subtitle(label, lang, new URL("http://foo.bar"), new URL("http://foo.baz"))
    }
  }
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
    def pos(pos: Int, max: Int): Int = if (pos < 0) max-pos else pos
    def apply(s: String): String
  }
  case class SubstrEndExpr(e: Expr, off: Int) extends SubstrExpr {
    def apply(s: String): String =
      s.substring(pos(off, s.length))
  }
  case class SubstrLenExpr(e: Expr, off: Int, len: Int) extends SubstrExpr {
    def apply(s: String): String = {
      val a = pos(off, s.length)
      val b = a + len 
      s.substring(a, b)
    }
  }
  case class SubstrPosExpr(e: Expr, off: Int, pos: Int) extends SubstrExpr {
    def apply(s: String): String = {
      val max = s.length
      val a = pos(off, max)
      val b = pos(pos, max)
      s.substring(a, b)
    }
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
      substr.apply( apply(e, elem, ctx) )
  }
}