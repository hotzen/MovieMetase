package moviemetase
package scraping

import java.net.URL
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import scala.collection._
import org.jsoup.select.Elements
import java.net.MalformedURLException


case class Selector(sel: String, idx: Option[Int], max: Option[Int]) extends Traceable with Logging {
  import language.implicitConversions
  implicit def JIterToIter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )

  val logID = "Selector(" + sel + 
                { idx match { case Some(x) => ", idx="+x case None => "" } } + 
                { max match { case Some(x) => ", max="+x case None => "" } } +
                ")"

  def apply(elem: Element): List[Element] = {
    val jiter = elem.select(sel)
    
    if (tracing && jiter.isEmpty)
      warn("NOTHING SELECTED")
    if (jiter.isEmpty)
      return Nil

    val selElems: List[Element] = jiter.iterator().toList

    if (tracing) {
      val total = selElems.length
      trace("selector matches " + total + " elements")
    }
    
    val idxd = idx match {
      case Some(x) => {
        val y =
          if (x > 0) x - 1
          else if (x < 0) selElems.length + x
          else throw new ExprEvalException("invalid Selector index " + x + " (1=first, 2=second, ..., n=last, -1=last, -2=second last, ..., -n=first )")

        try selElems(y) :: Nil
        catch { case e:IndexOutOfBoundsException => Nil } 
      }
      case None => selElems
    } 
    
    val maxd = max match {
      case Some(x) => idxd take x 
      case None => idxd
    }
    
    val result = maxd
    
    if (tracing) {
      val total = result.length
      val sb = new StringBuffer
      sb append "selected " append total append " elements:\n"
      result.zipWithIndex.foreach({case (elem,idx) =>
        sb append "*** " append (idx+1) append "/" append total append " *** "
        sb append elem.html append "\n"
      })
      sb append "*** END-OF-SELECT"
      trace(sb.toString)
    }
    
    result
  }
}


// **********************************************
// Expressions

class ExprEvalException(msg: String) extends Exception {
  var cause: Option[Throwable] = None

  def causedBy(t: Throwable): this.type = {
    cause = Some(t)
    this
  }
}

sealed trait Expr {
  def apply(elem: Element, ctx: Context[_]): String
}

case class LitExpr(s: String) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = s
}

case class ParamExpr(name: String, default: Option[String]) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = ctx.params.get(name) match {
    case Some(v) => v
    case None => default match {
      case Some(v) => v
      case None => throw new ExprEvalException("no default value specified for parameter '" + name + "'") 
    }
  }
}

case class VarExpr(name: String) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = ctx.vars.get(name) match {
    case Some(v) => v
    case None => throw new ExprEvalException("invalid variable '" + name + "'")
  }
}

case class SelExpr(selector: Selector) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    selector.apply(elem).map(_.text).mkString("")
  }
}

case class AttrExpr(name: String) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    val value = elem.attr(name)
    if (value == null) ""
    else value
  }
}

case class SelAttrExpr(selector: Selector, attr: String) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    selector.apply(elem).flatMap(selElem => selElem.attr(attr) match {
      case null => None
      case x => Some(x)
    }).mkString("")
  }
}

case class UrlExpr(e: Expr) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    val s = e.apply(elem, ctx)
    if (s.isEmpty) ""
    else try new URL(ctx.url, s).toExternalForm
         catch { case e:java.net.MalformedURLException => throw new ExprEvalException(e.getMessage).causedBy(e) }
  }
}

case class UrlEncExpr(e: Expr) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    val s = e.apply(elem, ctx)
    java.net.URLEncoder.encode(s, "UTF-8")
  }
}

case class ConcatExpr(a: Expr, b: Expr) extends Expr {
  def apply(elem: Element, ctx: Context[_]): String = {
    val sb = new StringBuilder
    sb append a.apply(elem, ctx)
    sb append b.apply(elem, ctx)
    sb.toString
  }
}

trait SubstrExpr extends Expr {
  def expr: Expr
  def absPos(pos: Int, len: Int): Int = if (pos < 0) len + pos else pos
  def apply(s: String): String
  
  def apply(elem: Element, ctx: Context[_]): String =
    apply( expr.apply(elem, ctx) )
}

case class SubstrEndExpr(expr: Expr, off: Int) extends SubstrExpr {
  def apply(s: String): String =
    try s.substring(absPos(off, s.length))
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(e.getMessage).causedBy(e) }
}

case class SubstrLenExpr(expr: Expr, off: Int, len: Int) extends SubstrExpr {
  def apply(s: String): String = {
    val a = absPos(off, s.length)
    val b = a + len
    try s.substring(a, b)
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(e.getMessage).causedBy(e) }
  }
}

case class SubstrPosExpr(expr: Expr, off: Int, pos: Int) extends SubstrExpr {
  def apply(s: String): String = {
    val len = s.length
    val a = absPos(off, len)
    val b = absPos(pos, len)
    try s.substring(a, b)
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(e.getMessage).causedBy(e) }
  }
}
// TODO RegexExpr


// **********************************************
// Context during processing of Steps

object Context {
  def defaultParams: Map[String, String] = {
    val locale = java.util.Locale.getDefault
    ("LANG" -> locale.getLanguage) ::
    ("LANG_ISO2" -> locale.getLanguage) ::
    ("LANG_ISO3" -> locale.getISO3Language) ::
    Nil
  }.toMap
}

case class Context[A](url: URL, extracts: List[(String, String)], factory: Factory[A], params: Map[String, String], vars: Map[String, String]) {

  def setURL(url: URL): Context[A] =
    Context(url, extracts, factory, params, vars)
  
  def addExtract(name: String, value: String): Context[A] =
    Context(url, (name, value) :: extracts, factory, params, vars)
  
  def setParam(name: String, value: String): Context[A] =
    Context(url, extracts, factory, params - name + (name -> value), vars)
    
  def setVar(name: String, value: String): Context[A] =
    Context(url, extracts, factory, params, vars - name + (name -> value))
}


// **********************************************
// Steps: consecutive steps of processing,
// each calling the next step (continuation)

trait Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A]
}

case class FinalStep[A]() extends Step[A] {
  def process(elem: Element, ctx: Context[A]): List[A] =
    ctx.factory.create( ctx.extracts.reverse )
  
  override def toString = "End"
}

case class BrowseStep[A](expr: Expr, postData: List[(String,String)], forceCtxUrl: Option[String], next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "Browse"
  
  def loadDoc(theUrl: URL): Document = new HtmlTask[Document] {
    def url = theUrl
        
    RequestMethod = if (postData.isEmpty) "GET" else "POST"
    RequestData = Some(os => {
      import java.io.OutputStreamWriter
      import java.net.URLEncoder
      val osw = new OutputStreamWriter(os)
      val data =
        for ((k,v) <- postData)
          yield URLEncoder.encode(k, "UTF-8") + "=" + URLEncoder.encode(v, "UTF-8")
      osw.write( data.mkString("&") )
      osw.flush()
      osw.close()
    })
    
    def processDocument(doc: Document): Document = doc
  }.submit().get() // enforce utiliziation of proper pool

  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = expr.apply(elem, ctx)
    val url = new URL(value)
    
    if (tracing)
      trace(url.toExternalForm)
      
    val ctxUrl = forceCtxUrl match {
      case Some(f) => new URL(f)
      case None => url
    }
      
    val doc = loadDoc(url)
    next.process(doc.body, ctx setURL ctxUrl)
  }
  
  override def toString = "Browse(" + expr + ")\n => " + next.toString
}

case class SelectStep[A](selector: Selector, next: Step[A]) extends Step[A] with Traceable with Logging {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
      
  val logID = "Select(" + selector + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    selector.trace(tracing).apply(elem).flatMap(selElem => next.process(selElem, ctx))
  }
  
  override def toString = "Select("+selector+")\n => " + next.toString
}

case class ExtractStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "Extract(" + name + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = expr.apply(elem, ctx)
    if (tracing)
      trace("'" + value + "'")
    
    if (value.isEmpty)
      next.process(elem, ctx)
    else
      next.process(elem, ctx.addExtract(name, value))
  }
    
  override def toString = "Extract(" + name + ", " + expr + ")\n => " + next.toString
}

case class BindVarStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "BindVar(" + name + ")"
  
  def process(elem: Element, ctx: Context[A]): List[A] = {
    val value = expr.apply(elem, ctx)
    if (tracing)
      trace("'" + value + "'")
    next.process(elem, ctx setVar (name, value))
  }
    
  override def toString = "BindVar(" + name + ", " + expr + ")\n => " + next.toString
}


trait Factory[A] {
  def create(extracts: List[(String,String)]): List[A]
}

trait Scraper[A] extends Traceable {
  def start: Step[A]
  def factory: Factory[A]
  
  def execute(params: List[(String, String)], startElem: Element, startURL: URL): List[A] = {
    val paramsMap = params.toMap ++ Context.defaultParams
    val varsMap = Map[String, String]()

    val ctx = Context[A](startURL, Nil, factory, paramsMap, varsMap)
    start.process(startElem, ctx)
  }
  
  override def toString = "Scraper: " + start.toString
}

trait PageScraper[A] extends Scraper[A] {

  def pageBodyTask(page: URL) = new HtmlTask[Element] {
    def url = page
    def processDocument(doc: Document): Element = doc.body
  }
  
  def scrapePage(page: java.net.URL): List[A] = {
    val params = ("PAGE" -> page.toExternalForm) :: Nil
    
    val startElem = pageBodyTask(page).submit().get()
    val startURL  = page
    
    execute(params, startElem, startURL)
  }
  
  override def toString = "PageScraper: " + start.toString
}

trait SearchScraper[A] extends Scraper[A] {
  import org.jsoup.Jsoup
  
  def search(term: String): List[A] = {
    val params = ("SEARCH" -> term) :: Nil
        
    val startElem = Jsoup.parse( """<html><head></head><body></body></html>""" ).body
    val startURL = new URL("http://initial.net/") 

    execute(params, startElem, startURL)
  }
  
  override def toString = "SearchScraper: " + start.toString
}