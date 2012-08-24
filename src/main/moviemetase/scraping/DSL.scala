package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers
import scala.util.matching.Regex
import java.net.MalformedURLException
import java.util.regex.Pattern

object DSL extends RegexParsers with PackratParsers {
  
  // haskell style comments
  override protected val whiteSpace = """(\s|\Q-- \E.*)+""".r
  
  // ############################################
  // basic
  
  val quotedRegex = """"([^"]*)"""".r
  val quoted = regexMatch(quotedRegex) ^^ { case m => m.group(1) }
  val unquoted = """[^\s\Q"[]()+\E]+""".r
  val value = quoted | unquoted
  
  val int = """-?\d+""".r ^^ { d => d.toInt }
  
  val selectorIdxFirst = "FIRST" ^^^ 1
  val selectorIdxLast = "LAST" ^^^ -1
  val selectorIdxNum = "#" ~> int ^^ {
    case i => i.toInt
  }
  val selectorIdx = selectorIdxFirst | selectorIdxLast | selectorIdxNum
  
  val selectorMax = "MAX" ~> int
    
  val selector = value ~ opt(selectorIdx) ~ opt(selectorMax) ^^ {
    case s ~ idx ~ max =>
      Selector(s, idx, max) 
  }
  
  
  // ############################################
  // expressions
  val literalExpr = value ^^ { case s => LitExpr(s) }
  
  val paramName = """[A-Z_]+""".r
  val paramDefault = "DEFAULT" ~> value
  val paramExpr = "<" ~ paramName ~ ">" ~ opt(paramDefault) ^^ {
    case _ ~ name ~ _ ~ default => ParamExpr(name, default)
  }
  
  val varName = """[A-Z]+""".r
  val varExprParser = "$" ~> varName
  val varExpr = varExprParser ^^ {
    case name => VarExpr(name)
  }
  
  val selExpr = "SELECT" ~> selector ^^ {
    case sel => SelExpr(sel)
  }
  
  val attrExpr = "ATTRIBUTE" ~> value ^^ {
    case name => AttrExpr(name)
  }
  
  val selAttrExpr: PackratParser[Expr] = "SELECT" ~ selector ~ "ATTRIBUTE" ~ value ^^ {
    case _ ~ sel ~ _ ~ attr => SelAttrExpr(sel, attr)
  }
  
  val parensExpr: Parser[Expr] = "(" ~> expr <~ ")"
  
  val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ {
    case e1 ~ _ ~ e2 => ConcatExpr(e1, e2)
  }

  val urlExpr: PackratParser[Expr] = expr <~ "AS-URL" ^^ {
    case e => UrlExpr(e)
  }
  
  val urlEncExpr: PackratParser[Expr] = expr <~ "URL-ENCODED" ^^ {
    case e => UrlEncExpr(e)
  }

  val substrRegex = """(-?\d+)(([,-])(\d+))?""".r
  val substrExpr: PackratParser[Expr] = expr ~ "[" ~ regexMatch(substrRegex) ~ "]" ^^ {
    case e ~ _ ~ m ~ _ => {
      val off = m.group(1).toInt
      m.group(3) match {
        case null => SubstrEndExpr(e, off)
        case "-"  => SubstrPosExpr(e, off, m.group(4).toInt)
        case ","  => SubstrLenExpr(e, off, m.group(4).toInt) 
      }
    }
  }
  
  // TODO 
//  val regexExpr: PackratParser[Expr] = expr ~ "REGEX" ~ quoted ^^ {
//    case e ~ _ ~ p =>
//      RegexExpr(e, p)
//  }
  
//  val scriptRegex: PackratParser[Expr] = "{{{" ~ """[^\}]+""".r ~ "}}}" ^^ {
//    case _ ~ script ~ _ =>
//      ScriptExpr(script)
//  }

  val basicExpr = paramExpr | varExpr | selAttrExpr | selExpr | attrExpr | literalExpr
  val postfixExpr: PackratParser[Expr] = substrExpr | urlEncExpr | urlExpr
  val binExpr: PackratParser[Expr] = concatExpr
  val expr: PackratParser[Expr] = postfixExpr | binExpr | parensExpr | basicExpr


  // ############################################
  // steps
  val trace = opt("TRACE") ^^ { _.isDefined }
  
  val browseAsUrl = "AS" ~> value
  val browsePostData = "POST" ~> rep1(value ~ "=" ~ value)
  val browseStep: Parser[Step[_]] = trace ~ "BROWSE" ~ expr ~ opt(browseAsUrl) ~ opt(browsePostData) ~ step ^^ {
    case t ~ _ ~ e ~ url ~ pd ~ next => {
      val postData = pd match {
        case Some(xs) => for (x <- xs) yield x match { case k ~ _ ~ v => (k, v)  }
        case None => Nil
      }
      BrowseStep(e, postData, url, next).trace(t)
    }
  }
  
  
  val selectStep: Parser[Step[_]] = trace ~ "SELECT" ~ selector ~ step ^^ {
    case t ~ _ ~ sel ~ next =>
      SelectStep(sel, next).trace(t)
  }
  
  val extractWhat = """[a-zA-Z0-9_\-/]+""".r 
  val extractStep: Parser[Step[_]] = trace ~ "EXTRACT" ~ extractWhat ~ expr ~ step ^^ {
    case t ~ _ ~ what ~ e ~ next =>
      ExtractStep(what, e, next).trace(t)
  }
  
  val bindVarStep: Parser[Step[_]] = trace ~ "SET" ~ varExprParser ~ "=" ~ expr ~ step ^^ {
    case t ~ _ ~ name ~ _ ~ e ~ next =>
      BindVarStep(name, e, next).trace(t)
  } 
  
  val finalStep: Parser[Step[_]] = "END" ^^^ FinalStep()
  
  val step: Parser[Step[_]] = browseStep | selectStep | extractStep | bindVarStep | finalStep  

  
  // ############################################
  // scrapers
  val subtitleScraper: Parser[Scraper[_]] = trace ~ "SCRAPE" ~ "SUBTITLES" ~ "ON" ~ value ~ step ^^ {
    case t ~ _ ~ x ~ _ ~ label ~ step =>
      SubtitleScraper(label, step.asInstanceOf[Step[MovieInfos.Subtitle]]).trace(t)
  }
  
  val subtitleSearcher: Parser[Scraper[_]] = trace ~ "SEARCH" ~ "SUBTITLES" ~ "ON" ~ value ~ step ^^ {
    case t ~ _ ~ x ~ _ ~ label ~ step =>
      SubtitleSearcher(label, step.asInstanceOf[Step[MovieInfos.Subtitle]]).trace(t)
  }

  val scraper: Parser[Scraper[_]] = subtitleScraper | subtitleSearcher 

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