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
    
    val test = """
TRACE search IMDB for Movies
  using http://www.imdb.com/find?s=tt&q=%term in HTML
    extract "
      for
        $a in //xhtml:a[starts-with(@href, '/title/')]
      where
        count( local:sub-elements( $a ) ) eq 0 (: only links containing no sub-elements :) 
      return
        <movie>
          <title>{ local:clean-text( $a ) }</title>
          <page>http://www.imdb.com{data($a/@href)}</page>
        </movie>"
      where xhtml is namespace http://www.w3.org/1999/xhtml
"""
       
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

class SourceParser extends RegexParsers {
  
  def pQuoted: Parser[String]   = "\"" ~> """[^"]+""".r <~ "\""
  def pUnquoted: Parser[String] = """[^\s]+""".r
  
  def pExpr: Parser[String] = pQuoted | pUnquoted
  def pURL: Parser[String] = pQuoted | pUnquoted
    
  def pSourceCategory: Parser[String] = pQuoted | pUnquoted
  def pSourceName: Parser[String] = pQuoted | pUnquoted

  def pContentEncoding: Parser[String] = """[a-zA-Z0-9-]+""".r <~ "encoded"
  
  def pContentHtml: Parser[ContentHandler] = "HTML" ^^^ ContentHandler.HTML()
  def pContentXml:  Parser[ContentHandler] = "XML"  ^^^ ContentHandler.XML()
  def pContentHandler: Parser[ContentHandler] = pContentHtml | pContentXml // | contentJson
  
  def pQuality: Parser[Int] = "of" ~> "quality" ~> """[0-9]+""".r ^^ (x => x.toInt) 
  
  def pNamespacePrefix: Parser[String] = pQuoted | pUnquoted
  def pNamespaceURI: Parser[String] = pQuoted | pUnquoted
  
  def pNamespaceContext: Parser[List[(String,String)]] =
    repsep( pNamespacePrefix ~ "is" ~ opt("namespace") ~ pNamespaceURI ^^ { case prefix ~ _is ~ _namespace ~ uri => (prefix, uri) }, "and")
   
  
  def pXPath: Parser[XPathT] = pExpr ~ opt("where" ~> pNamespaceContext) ^^ {
      case expr ~ optCtx => {
        val ctx = optCtx match {
          case Some(xs) => xs
          case None     => Nil
        }
        XPathT(expr, ctx)
      }
    }
 
  def pXQuery: Parser[XQueryT] = pExpr ~ opt("where" ~> pNamespaceContext) ^^ {
      case expr ~ optCtx => {
        val ctx = optCtx match {
          case Some(xs) => xs
          case None     => Nil
        }
        XQueryT(expr, ctx)
      }
    }
  
  def pRestrictions: Parser[List[XPathT]] =
    opt("restrict" ~> "to" ~> repsep(pXPath, "then")) ^^ {
      case Some(restricts) => restricts
      case None            => Nil
    }
  
  def pExtraction: Parser[XQueryT] = "extract" ~> pXQuery
  
//  def pModelName: Parser[String] = pQuoted | pUnquoted
//  def pModel
      
//  def pBindings: Parser[List[ModelBinding]] = "fetch" ~> 
      
      
//  def pDefinition: Parser[]
      
  def pSourceConfig: Parser[SourceConfiguration] = (
      pURL ~ "in" ~ opt(pContentEncoding) ~ pContentHandler
    ~ opt(pQuality)
    ~ pRestrictions
    ~ pExtraction
    ) ^^ {
      case url ~ _in ~ optEnc ~ handler ~ optQuality ~ restricts ~ extract => {
        handler.encoding = optEnc
        val transforms = restricts ++ List(extract)
        SourceConfiguration(url, handler, optQuality, transforms)
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
