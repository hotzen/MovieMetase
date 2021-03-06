package moviemetase
package extraction

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import java.net.URL
import java.net.MalformedURLException

sealed trait ExtractionException extends Throwable

class SelectorException(sel: Selector, msg: String, cause: Throwable) extends Exception(msg, cause) with ExtractionException

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
          else throw new SelectorException(this, "invalid Selector index " + x, null)

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

class ExprEvalException(expr: Expr, msg: String, cause: Throwable) extends Exception(msg, cause) with ExtractionException

sealed trait Expr {
  def eval(ctx: Context[_]): String
}

case class LitExpr(s: String) extends Expr {
  def eval(ctx: Context[_]): String = s
}

case class ParamExpr(name: String, default: Option[String]) extends Expr {
  def eval(ctx: Context[_]): String = ctx.params.get(name) match {
    case Some(v) => v
    case None => default match {
      case Some(v) => v
      case None => throw new ExprEvalException(this, "undefined parameter '" + name + "' without default value", null) 
    }
  }
}

case class VarExpr(name: String) extends Expr {
  def eval(ctx: Context[_]): String = ctx.vars.get(name) match {
    case Some(v) => v
    case None => throw new ExprEvalException(this, "invalid variable '" + name + "'", null)
  }
}

case class SelExpr(selector: Selector, sep: String) extends Expr {
  def eval(ctx: Context[_]): String = {
    selector(ctx.elem).map(_.text).mkString(sep)
  }
}

case class AttrExpr(name: String) extends Expr {
  def eval(ctx: Context[_]): String = {
    val value = ctx.elem.attr(name)
    if (value == null) ""
    else value
  }
}

case class SelAttrExpr(selector: Selector, attr: String, sep: String) extends Expr {
  def eval(ctx: Context[_]): String = {
    selector(ctx.elem).flatMap(selElem => selElem.attr(attr) match {
      case null => None
      case x => Some(x)
    }).mkString(sep)
  }
}

case class UrlExpr(e: Expr) extends Expr {
  def eval(ctx: Context[_]): String = {
    val s = e.eval(ctx)
    if (s.isEmpty) ""
    else try new URL(ctx.url, s).toExternalForm
         catch { case e:java.net.MalformedURLException => throw new ExprEvalException(this, e.getMessage, e) }
  }
}

case class UrlEncExpr(e: Expr) extends Expr {
  def eval(ctx: Context[_]): String = {
    val s = e.eval(ctx)
    java.net.URLEncoder.encode(s, "UTF-8")
  }
}

case class ConcatExpr(a: Expr, b: Expr) extends Expr {
  def eval(ctx: Context[_]): String = {
    val sb = new StringBuilder
    sb append a.eval(ctx)
    sb append b.eval(ctx)
    sb.toString
  }
}

trait SubstrExpr extends Expr {
  def expr: Expr
  def absPos(pos: Int, len: Int): Int = if (pos < 0) len + pos else pos
  def eval(s: String): String
  
  def eval(ctx: Context[_]): String = eval( expr.eval(ctx) )
}

case class SubstrEndExpr(expr: Expr, off: Int) extends SubstrExpr {
  def eval(s: String): String =
    try s.substring(absPos(off, s.length))
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(this, e.getMessage, e) }
}

case class SubstrLenExpr(expr: Expr, off: Int, len: Int) extends SubstrExpr {
  def eval(s: String): String = {
    val a = absPos(off, s.length)
    val b = a + len
    try s.substring(a, b)
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(this, e.getMessage, e) }
  }
}

case class SubstrPosExpr(expr: Expr, off: Int, pos: Int) extends SubstrExpr {
  def eval(s: String): String = {
    val len = s.length
    val a = absPos(off, len)
    val b = absPos(pos, len)
    try s.substring(a, b)
    catch { case e:IndexOutOfBoundsException => throw new ExprEvalException(this, e.getMessage, e) }
  }
}
// TODO RegexExpr


// **********************************************
// Context during executeing of Steps

class ContextException(ctx: Context[_], msg: String, cause: Throwable) extends Exception(msg) with ExtractionException

object Context {
  def defaultParams: Map[String, String] = {
    val locale = java.util.Locale.getDefault
    ("LANG" -> locale.getLanguage) ::
    ("LANG_ISO2" -> locale.getLanguage) ::
    ("LANG_ISO3" -> locale.getISO3Language) ::
    Nil
  }.toMap
}

case class Context[A](
    elems: List[Element],
    url: URL,
    extracts: List[(String, String)],
    params: Map[String, String],
    vars: Map[String, String],
    factory: Factory[A]
  ) {
  
  def elem: Element =
    try elems.head
    catch { case e:NoSuchElementException => throw new ContextException(this, "invalid Context-State, no elements", null) }
  
  def previous: Context[A] =
    Context(elems.tail, url, extracts, params, vars, factory)
  
  def setElem(elem: Element): Context[A] =
    Context(elem :: elems, url, extracts, params, vars, factory)
   
  def setURL(url: URL): Context[A] =
    Context(elems, url, extracts, params, vars, factory)
  
  def addExtract(name: String, value: String): Context[A] =
    Context(elems, url, (name, value) :: extracts, params, vars, factory)
  
  def setParam(name: String, value: String): Context[A] =
    Context(elems, url, extracts, params + (name -> value), vars, factory)
    
  def setVar(name: String, value: String): Context[A] =
    Context(elems, url, extracts, params, vars + (name -> value), factory)
}


// **********************************************
// Steps: consecutive steps of executeing,
// each calling the next step (continuation)

class StepExecException(step: Step[_], msg: String, cause: Throwable) extends Exception(msg, cause) with ExtractionException

sealed trait Step[A] {
  def execute(ctx: Context[A]): List[A]
}

case class FinalStep[A]() extends Step[A] {
  def execute(ctx: Context[A]): List[A] =
    ctx.factory.create( ctx.extracts.reverse )
  
  override def toString = "End"
}

case class GetStep[A](expr: Expr, forceCtxUrl: Option[String], next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "GET"
  
  def loadDoc(theUrl: URL): Document = new HtmlTask[Document] {
    def url = theUrl
    RequestMethod = "GET"
    def processDocument(doc: Document): Document = doc
  }.submit().get() // enforce utiliziation of proper pool

  def execute(ctx: Context[A]): List[A] = {
    val value = expr.eval(ctx)
    val url = new URL(value)

    if (tracing)
      trace(url.toExternalForm)

    val ctxUrl = forceCtxUrl match {
      case Some(f) => new URL(f)
      case None => url
    }
      
    val doc = loadDoc(url)
    next.execute(ctx.setElem(doc.body).setURL(ctxUrl))
  }
  
  override def toString = "GET(" + expr + ")\n => " + next.toString
}

case class PostStep[A](expr: Expr, data: List[(String,String)], forceCtxUrl: Option[String], next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "POST"
  
  def loadDoc(theUrl: URL): Document = new HtmlTask[Document] {
    def url = theUrl
    
    RequestMethod = "POST"
    RequestData = Some(os => {
      import java.io.OutputStreamWriter
      import java.net.URLEncoder
      val osw = new OutputStreamWriter(os)
      val encData =
        for ((k,v) <- data)
          yield URLEncoder.encode(k, "UTF-8") + "=" + URLEncoder.encode(v, "UTF-8")
      osw.write( encData.mkString("&") )
      osw.flush()
      osw.close()
    })
    
    def processDocument(doc: Document): Document = doc
  }.submit().get() // enforce utiliziation of proper pool

  def execute(ctx: Context[A]): List[A] = {
    val value = expr.eval(ctx)
    val url = new URL(value)

    if (tracing)
      trace(url.toExternalForm)

    val ctxUrl = forceCtxUrl match {
      case Some(f) => new URL(f)
      case None => url
    }
      
    val doc = loadDoc(url)
    next.execute(ctx.setElem(doc.body).setURL(ctxUrl))
  }
  
  override def toString = "POST(" + expr + ", {" + data.mkString(";") + "})\n => " + next.toString
}

case class SelectStep[A](selector: Selector, next: Step[A]) extends Step[A] with Traceable with Logging {
  import language.implicitConversions
  implicit def jiter[A](jiter: java.util.Iterator[A]): Iterator[A] =
    scala.collection.convert.Wrappers.JIteratorWrapper[A]( jiter )
      
  val logID = "Select(" + selector + ")"
  
  def execute(ctx: Context[A]): List[A] = {
    selector.trace(tracing)(ctx.elem).
      flatMap(selElem => next.execute(ctx.setElem(selElem)))
  }
  
  override def toString = "Select("+selector+")\n => " + next.toString
}

case class DeselectStep[A](num: Int, next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "Deselect(" + num + ")"
  
  def execute(ctx: Context[A]): List[A] = {
    var newCtx = ctx.previous
    if (num > 1)
      for (i <- 2 to num)
        newCtx = newCtx.previous
    
    if (tracing)
      trace("now in scope: " + newCtx.elem.html)
    next.execute(newCtx)
  }
  
  override def toString = "Deselect(" + num + ")\n => " + next.toString
}

case class ExtractStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "Extract(" + name + ")"
  
  def execute(ctx: Context[A]): List[A] = {
    val value = expr.eval(ctx)
    if (tracing)
      trace("'" + value + "'")
    
    if (value.isEmpty)
      next.execute(ctx)
    else
      next.execute(ctx.addExtract(name, value))
  }
    
  override def toString = "Extract(" + name + ", " + expr + ")\n => " + next.toString
}

case class BindVarStep[A](name: String, expr: Expr, next: Step[A]) extends Step[A] with Traceable with Logging {
  val logID = "BindVar(" + name + ")"
  
  def execute(ctx: Context[A]): List[A] = {
    val value = expr.eval(ctx)
    if (tracing)
      trace("'" + value + "'")
    next.execute(ctx setVar (name, value))
  }
    
  override def toString = "BindVar(" + name + ", " + expr + ")\n => " + next.toString
}


trait Factory[A] extends Traceable {
  def create(extracts: List[(String,String)]): List[A]
}

sealed trait ExtractorParamType
object ExtractorParamType {
  case object Term extends ExtractorParamType
  case object Page extends ExtractorParamType
}

case class Extractor[A](id: String, domain: String, paramType: ExtractorParamType, paramName: String, start: Step[A], factory: Factory[A]) extends Traceable with Logging {
  val logID = "Extractor(" + id + ")"
  
  def execute(input: String): List[A] = {
    execute((paramName -> input) :: Nil)
  }

  def execute(params: List[(String, String)]): List[A] = {
    val startElem = org.jsoup.Jsoup.parse( """<html><head></head><body>undefined</body></html>""" ).body
    val startURL = new URL("http://undefined.net/")
    execute(params, startElem, startURL)
  }

  def execute(params: List[(String, String)], startElem: Element, startURL: URL): List[A] = {
    val paramsMap = params.toMap ++ Context.defaultParams
    val varsMap = Map[String, String]()

    val ctx = Context[A](startElem :: Nil, startURL, Nil, paramsMap, varsMap, factory.trace(tracing))
    start.execute(ctx)
  }
  
  override def toString = "Extractor(" + id + " @ " + domain + "):\n => " + start.toString
}