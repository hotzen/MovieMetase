package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  def quoted = """"[^"]*"""".r
  //def quoted = "\"" ~> """[^"]*""".r <~ "\""
  def unquoted = """[^\s"]+""".r
  def value = quoted | unquoted
  
  def int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  //def keyword = """[A-Z\-_]+""".r
    
  
  // ############################################
  // expressions
  def literalExpr = value ^^ { case s => LitExpr(s) }
  
  def varExpr = "<" ~> """[A-Z]+""".r <~ ">" ^^ { case s => VarExpr(s) }
  
  def selectExpr = "SELECT" ~> value ^^ { case s => SelExpr(s) }
  
  def attrExpr = "ATTRIBUTE" ~> value ^^ { case s => AttrExpr(s) }
  
  def selAttrExpr = "SELECT-ATTRIBUTE" ~> value ~ value ^^ { case s ~ a => SelAttrExpr(s, a) }
  
  // "foo"(1,4) // offset, length
  // "foo"(1-5) // offset, pos
  //def substrExpr = """"[^"]*"\((\d+)(([,-])(\d+))?\)""".r
  
  def normalExpr = varExpr | selectExpr | attrExpr | selAttrExpr | literalExpr
    
  lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }

  lazy val expr: PackratParser[Expr] = concatExpr | normalExpr
  
    
  // ############################################
  // steps 
  def browseStep: Parser[Step[_]] = "BROWSE" ~> expr ~ step ^^ { case e ~ next => BrowseStep(e, next) }
  
  def selectStep: Parser[Step[_]] = "SELECT" ~> value /* ~ ("MAX" ~> int | Int.MaxValue ) */ ~ step ^^ { case e ~ next => SelectStep(e, Int.MaxValue, next) }
  
  def extractWhat = """[a-zA-Z0-9_\-]+""".r 
  def extractStep: Parser[Step[_]] = "EXTRACT" ~> extractWhat ~ expr ~ step ^^ { case what ~ e ~ next => ExtractStep(what.toLowerCase, e, next) }
  
  def terminalStep: Parser[Step[_]] = "END" ^^^ TerminalStep()
  
  def step: Parser[Step[_]] = browseStep | selectStep | extractStep | terminalStep

  
  // ############################################
  // scrapers
  lazy val subtitlesScraper: PackratParser[Scraper[_]] = "SCRAPE" ~> "SUBTITLES" ~> "AT" ~> value ~ "BY" ~ value ~ step ^^
                                                           { case site ~ _ ~ author ~ step => SubtitleScraper(site, author, step.asInstanceOf[Step[MovieInfos.Subtitle]]) }  
  
  lazy val scraper: PackratParser[Scraper[_]] = subtitlesScraper

  def scrapers = rep1(scraper)
  
  def apply(s: String): ParseResult[List[Scraper[_]]] = parseAll(scrapers, s)
}