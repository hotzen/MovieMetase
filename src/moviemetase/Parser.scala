package moviemetase

import scala.util.parsing.combinator._
import nu.xom.XPathContext

object SourceParser {
  
  def main(args: Array[String]) = {

//    """
//TRACE search IMDB for Movies
//  using http://www.imdb.com/find?s=tt&q=%term in HTML
//    restrict to //xhtml:table//xhtml:td/xhtml:a
//      where xhtml is namespace http://www.w3.org/1999/xhtml
//    extract
//      "for $a in xhtml:a
//       return <movie>$a[title] - $a[@title] - $a/@title</movie>"
//      where xhtml is namespace http://www.w3.org/1999/xhtml
//"""
    
//      extract "
//      for
//        $a in //xhtml:a[starts-with(@href, '/title/')]
//      where
//        count($a/*) = 0 
//      return
//        <movie>
//          <title>{ local:clean-text( $a ) }</title>
//          <page>http://www.imdb.com{data($a/@href)}</page>
//        </movie>"
//      where xhtml is namespace http://www.w3.org/1999/xhtml
//"""
       
//    val clazzes = List(
//        "Movie"
//      , "moviemetase.Movie"
//      , "MovieInfos.Title"
//      , "moviemetase.MovieInfos.Title"
//    )
//    for (clazz <- clazzes) {
//      println( clazz + ": " + App.classExists("") )
//    }
//    System.exit(0)
      
    
    val test = """
TRACE search IMDB for Movies
  using http://www.imdb.com/find?s=tt&q=%term in HTML
    restrict
      // <div id="pagecontent"> // <table> // <td> as $CTX
       / <a href="/title/tt*"> as $LINK
    extract
      Movie.Title = text of $LINK
      
      if attribute "href" of $LINK matches "/title/tt[0-9]+" then
        Movie.Page = "http://www.imdb.com" + attribute "href" of $LINK
      endif

      Movie.AlternativeTitle = text of each $CTX // <p class="find-aka">
"""
      
// DSL-VERSUCH 2011-03-02, nicht funktionsfähig
     val test2 = """
search "IMDB" for "Movies"
  fetch    http://www.imdb.com/find?s=tt&q=$TERM
  restrict // <div id="pagecontent"> // <table> // <td>    as <<TD>>
            / <a href="/title/tt*">                        as <<A>>
  extract
    <<A>>[Movie.Title]<</A>>
    <<A href="Movie.Page">> prepend "http://www.imdb.com"
    
    <<TD>> // <p class="find-aka">[Movie.AlternativeTitle]</p> 
"""

    val p = new SourceParser
    
    println(test.trim)
    val parseRes = p.parseAll(p.pSources, test)
    println(parseRes)
    println()
    val srcs = parseRes.get
    val src = srcs.head
    
    val q = Query("Terminator", Some(1984))
//    val proxy = Some( new java.net.Proxy(java.net.Proxy.Type.HTTP, new java.net.InetSocketAddress("10.31.7.215", 3128)) )
    val proxy = None
    
    val searchRes = src.createSearcher(q, proxy).call
    
    println("results:")
    println( searchRes.mkString("\n") )
    println(".")
  }
  
  def loadFile(path: String) = {
    
  }
}

trait RestrictionPathComponent {
  def toXPath: String
}

case class RestrictionAxis(axis: String) extends RestrictionPathComponent {
  def toXPath: String = axis
}

case class RestrictionPredicateAttribute(name: String, value: Option[String]) {
  
  def toXPath: String = {
    val xp = new StringBuilder
    value match {
      case Some(v) => {
        if (v.startsWith("*") && v.endsWith("*")) {
          val vs = v.split('*')
          xp append "contains("
          xp append "@" append name
          xp append ", "
          xp append "'" append vs(1) append "'"
          xp append ")"
        } else if (v.startsWith("*")) {
          val vs = v.split('*')
          xp append "starts-with("
          xp append "@" append name
          xp append ", "
          xp append "'" append vs(1) append "'"
          xp append ")"
        } else if (v.endsWith("*")) {
          val vs = v.split('*')
          xp append "ends-with(" //XXX does not exist?
          xp append "@" append name
          xp append ", "
          xp append "'" append vs(1) append "'"
          xp append ")"
        } else {
          xp append "@" append name
          xp append "="
          xp append "'" append v append "'"
        }
      }
      case None => {
        xp append "@"
        xp append name
      }
    }
    xp.toString
  }
}

case class RestrictionPredicate(elem: String, attribs: List[RestrictionPredicateAttribute]) extends RestrictionPathComponent {
  def toXPath: String = {
    val xp = new StringBuilder
    xp append elem
    
    if (!attribs.isEmpty) {
      xp append "["
      xp append attribs.map(_.toXPath).mkString(" and ")
      xp append "]"
    }
    
    xp.toString
  }
}

case class Restriction(path: List[RestrictionPathComponent], varName: String) {
  def toXPath: String = path.map(_.toXPath).mkString(" ")
}

//
//case class ElementPredicate(name: String) extends Function[String,Boolean] {
//  def isWildcard: Boolean = (name == "*")
//  
//  override def apply(x: String): Boolean = isWildcard || (x == name)
//}
//
//case class AttributePredicate(name: String, p: String => Boolean) extends PartialFunction[String,Function[String,Boolean]] {
//  def isWildcard: Boolean = (name == "*")
//  
//  override def isDefinedAt(x: String): Boolean = isWildcard || (x == name)
//  
//  override def apply(x: String): Function[String, Boolean] = {
//    if (!isDefinedAt(x)) throw new Exception(this + " is not defined at " + x)
//    p
//  }
//}
//
//case class RestrictionPredicate(pElem: ElementPredicate, pAttribs: List[AttributePredicate]) {
//  def apply(elem: nu.xom.Element): Boolean = {
//    import XOM._
//    
//    if (!pElem(elem.name))
//      return false
//    
//    // check each attribute with each predicate
//    elem.attributes.forall(attrib => pAttribs.forall(p => {
//      
//      // predicate is defined for this attribute
//      if (p.isDefinedAt(attrib.name))
//        p(attrib.name)(attrib.value)
//      
//      // predicate is undefined for this attribute
//      else
//        true
//    }))
//  }
//}



class SourceParser extends RegexParsers {
  
  def pQuoted: Parser[String]   = "\"" ~> """[^"]+""".r <~ "\""
  def pUnquoted: Parser[String] = """[^\s]+""".r
  
  //def pExpr: Parser[String] = pQuoted | pUnquoted
  def pURL: Parser[String] = pQuoted | pUnquoted
  
  def pSourceCategory: Parser[String] = pQuoted | pUnquoted
  def pSourceName: Parser[String] = pQuoted | pUnquoted

  def pContentEncoding: Parser[String] = """[a-zA-Z0-9-]+""".r <~ "encoded"
  
  def pContentHtml: Parser[ContentHandler] = "HTML" ^^^ ContentHandler.HTML()
  def pContentXml:  Parser[ContentHandler] = "XML"  ^^^ ContentHandler.XML()
  def pContentHandler: Parser[ContentHandler] = pContentHtml | pContentXml // | contentJson
  
  def pQuality: Parser[Int] = "of" ~> "quality" ~> """[0-9]+""".r ^^ (x => x.toInt) 
  
  def pRestrPredElemName: Parser[String] = """[a-zA-Z0-9-_]+""".r
  
  def pRestrPredAttribName: Parser[String] = """[^\s=>]+""".r
  def pRestrPredAttribValue: Parser[String] = pQuoted
  
  def pRestrPredAttrib: Parser[RestrictionPredicateAttribute] = (pRestrPredAttribName ~ opt("=" ~> pRestrPredAttribValue)) ^^ {
    case name ~ optValue =>
      RestrictionPredicateAttribute(name, optValue)
  }
    
  def pRestrPredicate: Parser[RestrictionPredicate] = ("<" ~> pRestrPredElemName ~ rep(pRestrPredAttrib) <~ ">") ^^ {
    case elem ~ attribs =>
      RestrictionPredicate(elem, attribs)
  }
    
  def pRestrAxis: Parser[RestrictionAxis] = ("//" | "/") ^^ {
//    case "/" =>
//      RestrictionAxisChildren
//    
//    case "//" =>
//    
    case axis =>
      RestrictionAxis(axis)
  }
  
  
  def pRestrAxisPredicate: Parser[List[RestrictionPathComponent]] = pRestrAxis ~ pRestrPredicate ^^ {
    case axis ~ pred =>
      axis :: pred :: Nil
  }
  
  def pRestrVarName: Parser[String] = "as" ~> "$" ~> """[A-Z0-9_]+""".r
    
  def pRestriction: Parser[Restriction] = rep(pRestrAxisPredicate) ~ pRestrVarName ^^ {
    case axisPreds ~ varName =>
      Restriction( axisPreds.flatten, varName )
  }
  
  def pRestrictions: Parser[List[Restriction]] = "restrict" ~> rep( pRestriction )
  def pExtractions: Parser[String] = "extract" ~> """.*""".r
  
  def pSourceConfig: Parser[SourceConfiguration] = (
      pURL ~ "in" ~ opt(pContentEncoding) ~ pContentHandler
    ~ opt(pQuality)
    ~ pRestrictions
    ~ pExtractions
    ) ^^ {
      case url ~ _in ~ optEnc ~ handler ~ optQuality ~ restricts ~ extracts => {
        handler.encoding = optEnc
        
        println("Restrictions: " + restricts.mkString("\n"))
        System.exit(0)
        
        SourceConfiguration(url, handler, optQuality, null)
    }}
  
  def pSource: Parser[Source] =
      opt("TRACE") ~ "search" ~ pSourceName ~ "for" ~ pSourceCategory ~ "using" ~ repsep(pSourceConfig, "or") ^^ {
      case optTrace ~ _search ~ srcName ~ _for ~ srcCat ~ _with ~ configs => {
        val trace = optTrace match {
          case Some(_) => true
          case None    => false
        }
        Source(srcName, configs, srcCat, trace)
      }
    }
 
  def pSources: Parser[List[Source]] = repsep(pSource, ";")
}
