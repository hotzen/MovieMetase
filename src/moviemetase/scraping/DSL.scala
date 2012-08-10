package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers
import scala.util.matching.Regex

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  
  // ############################################
  // basic
  
  val quoted   = """"[^"]*"""".r
  val unquoted = """[^\s"]+""".r
  val value    = quoted | unquoted
  
  val int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  
  // ############################################
  // expressions
  val literalExpr = value ^^ { case s => LitExpr(s) }
  
  val varExpr = "<" ~> """[A-Z]+""".r <~ ">" ^^ { case s => VarExpr(s) }
  
  val selExpr = "SELECT" ~> value ^^ { case s => SelExpr(s) }
  
  val attrExpr = "ATTRIBUTE" ~> value ^^ { case s => AttrExpr(s) }
  
  val selAttrExpr = "SELECT-ATTRIBUTE" ~> value ~ value ^^ { case s ~ a => SelAttrExpr(s, a) }
    
  val easyExpr = varExpr | selAttrExpr | selExpr | attrExpr
    
  lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }

  val substrRegex = """\((\d+)(,|-)(\d+)?\)""".r 
  lazy val substrExpr: PackratParser[Expr] = expr ~ regexMatch(substrRegex) ^^ { case e ~ m => {

      val off = m.group(1).toInt
      val op = m.group(2)
      val x = m.group(3).toInt
      
      if (op == ",")
        SubstrLenExpr(e, off, x)
      else
        SubstrPosExpr(e, off, x)
    }}
  
  
  lazy val expr: PackratParser[Expr] = concatExpr | easyExpr | literalExpr
  
    
  // ############################################
  // steps 
  val browseStep: Parser[Step[_]] = "BROWSE" ~> expr ~ step ^^ { case e ~ next => BrowseStep(e, next) }
  
  val selectStep: Parser[Step[_]] = "SELECT" ~> value /* ~ ("MAX" ~> int | Int.MaxValue ) */ ~ step ^^ { case e ~ next => SelectStep(e, Int.MaxValue, next) }
  
  val extractWhat = """[a-zA-Z0-9_\-]+""".r 
  val extractStep: Parser[Step[_]] = "EXTRACT" ~> extractWhat ~ expr ~ step ^^ { case what ~ e ~ next => ExtractStep(what.toLowerCase, e, next) }
  
  val terminalStep: Parser[Step[_]] = "END" ^^^ TerminalStep()
  
  val step: Parser[Step[_]] = browseStep | selectStep | extractStep | terminalStep

  
  // ############################################
  // scrapers
  val subtitlesScraper: PackratParser[Scraper[_]] = "SCRAPE" ~> "SUBTITLES" ~> "AT" ~> value ~ "BY" ~ value ~ step ^^
                                                           { case site ~ _ ~ author ~ step => SubtitleScraper(site, author, step.asInstanceOf[Step[MovieInfos.Subtitle]]) }  
  
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