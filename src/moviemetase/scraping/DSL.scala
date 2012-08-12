package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers
import scala.util.matching.Regex
import java.net.MalformedURLException
import java.util.regex.Pattern

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  // ############################################
  // basic
  
  val quotedRegex = """"([^"]*)"""".r
  val quoted = regexMatch(quotedRegex) ^^ { case m => m.group(1) }
  val unquoted = """[^\s\Q"[]()+\E]+""".r
  val value = quoted | unquoted
  
  val int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  
  // ############################################
  // expressions
  val literalExpr = value ^^ { case s => LitExpr(s) }
  
  val paramName = """[A-Z]+""".r
  val paramExpr = "<" ~> paramName <~ ">" ^^ {
    case name => ParamExpr(name)
  }
  
  val varName = """[A-Z]+""".r
  val varExprParser = "$" ~> varName
  val varExpr = varExprParser ^^ {
    case name => VarExpr(name)
  }
  
  val selExpr = "SELECT" ~> value ^^ {
    case name => SelExpr(name)
  }
  
  val attrExpr = "ATTRIBUTE" ~> value ^^ {
    case name => AttrExpr(name)
  }
  
  lazy val selAttrExpr: PackratParser[Expr] = "SELECT" ~> value ~ "ATTRIBUTE" ~ value ^^ {
    case s ~ _ ~ a => SelAttrExpr(s, a)
  }
  
  val parensExpr: Parser[Expr] = "(" ~> expr <~ ")"
  
  lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ {
    case e1 ~ _ ~ e2 => ConcatExpr(e1, e2)
  }

  lazy val urlExpr: PackratParser[Expr] = expr ~ "AS" ~ "URL" ^^ {
    case e ~ _ ~ _ => UrlExpr(e)
  }

  val substrRegex = """(-?\d+)(([,-])(\d+))?""".r
  lazy val substrExpr: PackratParser[Expr] = expr ~ "[" ~ regexMatch(substrRegex) ~ "]" ^^ {
    case e ~ _ ~ m ~ _ => {
      val off = m.group(1).toInt
      m.group(3) match {
        case null => SubstrEndExpr(e, off)
        case "-"  => SubstrPosExpr(e, off, m.group(4).toInt)
        case ","  => SubstrLenExpr(e, off, m.group(4).toInt) 
      }
    }
  }

  val basicExpr: PackratParser[Expr] = paramExpr | varExpr | selAttrExpr | selExpr | attrExpr | literalExpr
  val postfixExpr: PackratParser[Expr] = substrExpr | urlExpr
  val binExpr: PackratParser[Expr] = concatExpr
  val expr: PackratParser[Expr] = parensExpr | postfixExpr | binExpr | basicExpr


  // ############################################
  // steps
  val trace = opt("TRACE") ^^ { _.isDefined }
  
  val browseBase = "WITH" ~> "BASE" ~> value
  val browseStep: Parser[Step[_]] = trace ~ "BROWSE" ~ expr ~ opt(browseBase) ~ step ^^ {
    case t ~ _ ~ e ~ base ~ next =>
      BrowseStep(e, next, base).trace(t)
  }
  
  val selectMax = "MAX" ~> int ^^ { case i => i.toInt }
  val selectStep: Parser[Step[_]] = trace ~ "SELECT" ~ value ~ opt(selectMax) ~ step ^^ {
    case t ~ _ ~ e ~ max ~ next =>
      SelectStep(e, max.getOrElse(Int.MaxValue), next).trace(t)
  }
  
  val extractWhat = """[a-zA-Z0-9_\-/]+""".r 
  val extractStep: Parser[Step[_]] = trace ~ "EXTRACT" ~ extractWhat ~ expr ~ step ^^ {
    case t ~ _ ~ what ~ e ~ next =>
      ExtractStep(what, e, next).trace(t)
  }
  
  val storeStep: Parser[Step[_]] = trace ~ ("STORE" | "REMEMBER") ~ varExprParser ~ expr ~ step ^^ {
    case t ~ _ ~ name ~ e ~ next =>
      StoreStep(name, e, next).trace(t)
  } 
  
  val finalStep: Parser[Step[_]] = "END" ^^^ FinalStep()
  
  val step: Parser[Step[_]] = browseStep | selectStep | extractStep | storeStep | finalStep  

  
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