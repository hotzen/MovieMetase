package moviemetase

import java.net.{URL, URLEncoder, Proxy}
import java.io.{PrintStream, InputStream}
import nu.xom.{Builder, Node, Element, Document, Namespace, XPathContext}
import org.xml.sax.helpers.XMLReaderFactory
import java.util.concurrent.Callable


sealed trait ContentHandler {
  var encoding: Option[String] = None
  def load(is: InputStream): Document
}

object ContentHandler {
  case class HTML() extends ContentHandler {
    def load(is: InputStream): Document = {
      val rdr = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser")
      val bld = new Builder( rdr )
      bld.build(is)
    }
    override def toString: String = "HTML/" + encoding
  }
  
  case class XML()  extends ContentHandler {
    def load(is: InputStream): Document = {
      val bld = new Builder()
      bld.build(is)
    }
    override def toString: String = "XML/" + encoding
  }
}

sealed trait Transformation {
  def transform(node: Node): List[Node]
}

case class XPathT(expr: String, context: List[(String,String)]) extends Transformation {
  import XOM._
  
  def transform(node: Node): List[Node] = context match {
    case Nil =>
      node.query(expr).toList
    
    case ctx :: ctxs => {
      val XPathCtx = new XPathContext(ctx._1, ctx._2)
      ctxs.foreach( ctx1 => XPathCtx.addNamespace(ctx1._1, ctx1._2) )
      
      node.query(expr, XPathCtx).toList
    }
  }
}

object XQueryT {
  val Predef = """
    declare variable $QUOTE := '"';
	declare function local:unquote($txt AS xs:string) AS xs:string {
	  return
        if ( contains($txt, $QUOTE) )
        then substring-before(substring-after($txt, $QUOTE), $QUOTE)
        else $txt
    }
    declare function local:clean-text($node) {
      let $txt := $node / text()
      return local:unquote($txt)
    };
    declare function local:sub-elements($node) {
      let $subElems := $node / * 
      return $subElems
    };
  """.trim + "\n"
}

case class XQueryT(_expr: String, context: List[(String,String)]) extends Transformation  {
  import nux.xom.xquery.XQuery
  import XOM._
  
  def expr: String = {
    val prolog = context.map(ctx =>
      """declare namespace %s = "%s"; """.format(ctx._1, ctx._2)
    ).mkString("", "\n", "\n")
    prolog + XQueryT.Predef + _expr
  }
  
  def transform(node: Node): List[Node] = {
    
    println(expr)
    
    val xq = new XQuery(expr, null)
    
    println( xq.explain )
    
    xq.execute(node).toNodes.toList
  }
}

object QueryVars {
  
  // var-indicator
  val IND = "%"
  
  def gen(q: Query): List[(String,Option[String])] = {
    val vs = new scala.collection.mutable.ListBuffer[(String,Option[String])]
    
    val term = q.term
    val year = q.year4C
    
    vs append ("$TERM" -> Some(term))
    vs append ("$YEAR" -> year)
    vs append ("$TERM_YEAR" -> {year match {
      case Some(y) => Some( term + " " + y )
      case None    => None
    }})
    // year in parens
    vs append ("$TERM_PYEAR" -> {year match {
      case Some(y) => Some( term + " (" + y + ")" )
      case None    => None
    }})
    vs.toList
  }
}


case class SourceConfiguration(urlTpl: String, handler: ContentHandler, presetQuality: Option[Int], transformations: List[Transformation]) {
  
  val trace: PrintStream = Console.out
  
  def quality: Int = presetQuality match {
    case Some(q) => q
    case None    => urlTpl.count(_ == '%')
  }
  
  // generates an URL if this SourceConfiguration is applicable for passed query
  def generateURL(q: Query): Option[URL] = {
    var url = urlTpl
    
    for ( (varName, optVarVal) <- QueryVars.gen(q)) optVarVal match {
      case Some(varVal) => url = url.replace(varName, URLEncoder.encode(varVal, "UTF-8"))
      case None =>
    }
    
    if (url contains QueryVars.IND)
      None
    else
      Some( new URL(url) )
  }
  
  
  def process(is: InputStream): List[Movie] = {
    import XOM._
    
    val doc = handler.load( is )
    //trace.println("XML:\n" + doc.toXML + "\n<<<END")
        
    // start with document-node to apply refinements
    var nodes: List[Node] = doc :: Nil
    
    // apply transformations
    for (t <- transformations) {
      trace.println("applying transformation %s".format(t))
      
      nodes = nodes.flatMap(node => t.transform(node) )
      
      trace.println("result of transformation: %s".format(nodes.map(_.toXML).mkString("\n")) + "\n\n")
    }
    
    Nil
  }
}


case class Source(name: String, configs: List[SourceConfiguration], category: String, var tracing: Boolean = false) {
    
  val trace: PrintStream = Console.out
  
  def orderedConfigs: List[SourceConfiguration] =
    configs.sortWith( (c1,c2) => c1.quality > c2.quality )
  
  def generateURLs(q: Query): List[(SourceConfiguration, URL)] =
    orderedConfigs.flatMap(config => {
      config generateURL q match {
        case Some(url) => Some(config, url)
        case None      => None
      }
    })
  
  def generateURL(q: Query): Option[(SourceConfiguration, URL)] =
    generateURLs(q) match {
      case (cfg, url) :: _ => Some(cfg, url)
      case Nil             => None
    }
  
  def createSearcher(q: Query, proxy: Option[Proxy]) = new Callable[List[Movie]] {
        
    def loadURL(url: URL): InputStream = {
      trace.println("loading %s ...".format(url))
      
      val conn = proxy match {
        case Some(p) => url.openConnection(p)
        case None    => url.openConnection
      }
      
      conn setUseCaches false
      conn setAllowUserInteraction false
      conn setDoInput true
      conn setDoOutput false
      
      conn.setRequestProperty("Accept",     "application/xml,application/xhtml+xml,text/html,text/plain")
      conn.setRequestProperty("Referer",    "http://" + url.getHost + "/")
      conn.setRequestProperty("User-Agent", "Mozilla/5.0") // (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.7 (KHTML, like Gecko) Chrome/7.0.517.44 Safari/534.7")

      conn.connect
      conn.getInputStream
    }
        
    def call(): List[Movie] = {
      val (srcCfg, url) = generateURL(q) match {
        case Some( tpl ) => tpl 
        case None => throw new Exception("no applicable URL could be generated for " + q)
      }
      trace.println("using %s".format(srcCfg))
      
      val is = loadURL(url)
      
      trace.println("processing input-stream ...")
      srcCfg.process( is )
    }
  }
  
  def createFetcher(m: Movie) = new Callable[Option[Movie]] {
    def call(): Option[Movie] = {
      //TODO
      None
    }
  }
}