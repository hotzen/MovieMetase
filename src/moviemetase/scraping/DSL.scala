package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers
import scala.util.matching.Regex
import java.net.MalformedURLException

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  
  // ############################################
  // basic
  
  val quotedRegex = """"([^"]*)"""".r
  val quoted = regexMatch(quotedRegex) ^^ { case m => m.group(1) }
  val unquoted = """[^\s"]+""".r
  val value = quoted | unquoted
  
  val int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  
  // ############################################
  // expressions
  val literalExprParser = value ^^ { case s => LitExpr(s) }
  
  val paramName = """[A-Z]+""".r
  val paramExpr = "<" ~> paramName <~ ">"
  val paramExprParser = paramExpr ^^ { case name => ParamExpr(name) }
  
  val varName = """[A-Z]+""".r
  val varExpr = "$" ~> varName 
  val varExprParser = varExpr ^^ { case name => VarExpr(name) }
  
  val selExpr = "SELECT" ~> value 
  val selExprParser = selExpr ^^ { case name => SelExpr(name) }
  
  val attrExpr = "ATTRIBUTE" ~> value
  val attrExprParser = attrExpr ^^ { case name => AttrExpr(name) }
  
//  val selAttrExpr = "SELECT-ATTRIBUTE" ~> value ~ value ^^ { case s ~ a => SelAttrExpr(s, a) }
//    
//  val easyExpr = varExpr | selAttrExpr | selExpr | attrExpr
//    
//  lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }
  
  lazy val selAttrExprParser: PackratParser[Expr] = "SELECT" ~> value ~ "ATTRIBUTE" ~ value ^^ {
    case s ~ _ ~ a => SelAttrExpr(s, a)
  }
      
  lazy val concatExprParser: PackratParser[Expr] = expr ~ "+" ~ expr ^^ {
    case e1 ~ _ ~ e2 => ConcatExpr(e1, e2)
  }
  
  lazy val urlExprParser: PackratParser[Expr] = expr ~ "AS" ~ "URL" ^^ {
    case e ~ _ ~ _ => UrlExpr(e)
  }
  
//  val substrRegex = """\((\d+)(([,-])((\d+)))?\)""".r 
//  lazy val substrExprParser: PackratParser[Expr] = expr ~ regexMatch(substrRegex) ^^ {
//    case e ~ m => {
//      for (i <- 0 until m.groupCount)
//        println(i+":  " +m.group(i) )
//      
//      val off = m.group(1).toInt
//      m.group(3) match {
//        case null => SubstrEndExpr(e, off)
//        case "-"  => SubstrPosExpr(e, off, m.group(4).toInt)
//        case ","  => SubstrLenExpr(e, off, m.group(4).toInt) 
//      }
//    }
//  }
  
  val basicExprParser = paramExprParser | varExprParser | selAttrExprParser | selExprParser | attrExprParser

//  val expr = substrExprParser | concatExprParser | basicExprParser | literalExprParser
  val expr = urlExprParser | concatExprParser | basicExprParser | literalExprParser
    
  // ############################################
  // steps
  val tracePrefix = opt("TRACE") ^^ { _.isDefined }
  
  val browseBase = "WITH" ~> "BASE" ~> value
  val browseStep: Parser[Step[_]] = tracePrefix ~ "BROWSE" ~ expr ~ opt(browseBase) ~ step ^^ {
    case t ~ _ ~ e ~ base ~ next =>
      BrowseStep(e, next, base).trace(t)
  }
  
  val selectMax = "MAX" ~> int ^^ { case i => i.toInt }
  val selectStep: Parser[Step[_]] = tracePrefix ~ "SELECT" ~ value ~ opt(selectMax) ~ step ^^ {
    case t ~ _ ~ e ~ max ~ next =>
      SelectStep(e, max.getOrElse(Int.MaxValue), next).trace(t)
  }
  
  val extractWhat = """[a-zA-Z0-9_\-/]+""".r 
  val extractStep: Parser[Step[_]] = tracePrefix ~ "EXTRACT" ~ extractWhat ~ expr ~ step ^^ {
    case t ~ _ ~ what ~ e ~ next =>
      ExtractStep(what, e, next).trace(t)
  }
  
  val storeStep: Parser[Step[_]] = tracePrefix ~ ("STORE" | "REMEMBER") ~ varExpr ~ expr ~ step ^^ {
    case t ~ _ ~ name ~ e ~ next =>
      StoreStep(name, e, next).trace(t)
  }
  
  val terminalStep: Parser[Step[_]] = "END" ^^^ TerminalStep()
  
  val step: Parser[Step[_]] = browseStep | selectStep | extractStep | storeStep | terminalStep  

  
  // ############################################
  // scrapers
  val subtitlesScraper: PackratParser[Scraper[_]] = "SCRAPE" ~> "SUBTITLES" ~> "ON" ~> value ~ step ^^ {
    case label ~ step =>
      SubtitleScraper(label, step.asInstanceOf[Step[MovieInfos.Subtitle]])
  }

  val scraper: PackratParser[Scraper[_]] = subtitlesScraper

  val scrapers = rep1(scraper)
  
  
  // ############################################
  // utils
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched, in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
          //Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }
  
  def apply(s: String): ParseResult[List[Scraper[_]]] = parseAll(scrapers, s)
}