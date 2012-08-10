package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  val quoted = """"[^"]*"""".r
  //def quoted = "\"" ~> """[^"]*""".r <~ "\""
  val unquoted = """[^\s"]+""".r
  val value = quoted | unquoted
  
  val int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  //def keyword = """[A-Z\-_]+""".r
    
  
  // ############################################
  // expressions
  val literalExpr = value ^^ { case s => LitExpr(s) }
  
  val varExpr = "<" ~> """[A-Z]+""".r <~ ">" ^^ { case s => VarExpr(s) }
  
  val selExpr = "SELECT" ~> value ^^ { case s => SelExpr(s) }
  
  val attrExpr = "ATTRIBUTE" ~> value ^^ { case s => AttrExpr(s) }
  
  val selAttrExpr = "SELECT-ATTRIBUTE" ~> value ~ value ^^ { case s ~ a => SelAttrExpr(s, a) }
    
  // "foo"(1,4) // offset, length
  // "foo"(1-5) // offset, pos
  //def substrExpr = """"[^"]*"\((\d+)(([,-])(\d+))?\)""".r
  
  val easyExpr = varExpr | selAttrExpr | selExpr | attrExpr
    
  lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }

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
  
  def apply(s: String): ParseResult[List[Scraper[_]]] = parseAll(scrapers, s)
}