package moviemetase
package scraping

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.PackratParsers

object DSL extends RegexParsers with PackratParsers {
  
  override protected val whiteSpace = """(\s|#.*)+""".r
  
  def quoted   = "\"" ~> """[^"]*""".r <~ "\""
  def unquoted = """\S+""".r
  def value    = quoted // | unquoted
  
  def int = """-?[0-9]+""".r ^^ { d => d.toInt }
  
  def keyword = """[A-Z\-_]+""".r
    
  
  // ############################################
  // expressions
  def literalExpr = value ^^ { case s => LiteralExpr(s) }
  
  def selectExpr = "SELECT" ~> value ^^ { case s => SelectExpr(s) }
  
  def attributeExpr = "ATTRIBUTE" ~> value ^^ { case s => AttributeExpr(s) }
  
  def selectAttributeExpr = "SELECT-ATTRIBUTE" ~> value ~ value ^^ { case s ~ a => SelectAttributeExpr(s, a) }
    
  def identExpr = "<" ~> """[A-Z]""".r <~ ">" ^^ { case s => IdentExpr(s) }
  
  //lazy val concatExpr: PackratParser[Expr] = expr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }
  //lazy val expr: PackratParser[Expr] = concatExpr | selectExpr | attributeExpr | selectAttributeExpr | literalExpr
  
  def normalExpr = selectExpr | attributeExpr | selectAttributeExpr | literalExpr
  def concatExpr: Parser[Expr] = normalExpr ~ "+" ~ expr ^^ { case e1 ~ _ ~ e2 => ConcatExpr(e1, e2) }
    
  def expr = normalExpr | concatExpr
  
    
  // ############################################
  // steps 
  lazy val browseStep: PackratParser[Step[_]] = "BROWSE" ~> expr ~ step ^^ { case e ~ next => BrowseStep(e, next) }
  
  lazy val selectStep: PackratParser[Step[_]] = "SELECT" ~> value /* ~ ("MAX" ~> int | Int.MaxValue ) */ ~ step ^^ { case e ~ next => SelectStep(e, Int.MaxValue, next) }
  
  lazy val extractStep: PackratParser[Step[_]] = "EXTRACT" ~> keyword ~ expr ~ step ^^ { case what ~ e ~ next => ExtractStep(what, e, next) }
  
  lazy val terminalStep: PackratParser[Step[_]] = "END" ^^^ TerminalStep()
  
  lazy val step: PackratParser[Step[_]] = browseStep | selectStep | extractStep | terminalStep

  
  // ############################################
  // scrapers
  
  lazy val subtitlesScraper: PackratParser[Scraper[_]] = "SCRAPE" ~> "SUBTITLES" ~> "AT" ~> value ~ "BY" ~ value ~ step ^^ { case site ~ _ ~ author ~ step => SubtitleScraper(site, author, step.asInstanceOf[Step[MovieInfos.Subtitle]]) }  
  
  lazy val scraper: PackratParser[Scraper[_]] = subtitlesScraper

  def scrapers = rep1(scraper)
  
  def apply(s: String): List[Scraper[_]] =
    parseAll(scrapers, s) match {
      case Success(t, _)     => t
      case f@NoSuccess(msg,_) => {
        println(f)
        Nil
      }
    }
}


object DSL_Test {
  val scraper1 = """
SCRAPE SUBTITLES AT "SubtitleSource.org" BY "fizzl@foo"
 
  BROWSE "http://www.subtitlesource.org/search/" + ATTRIBUTE "foo" + <QUERY> 
  SELECT "#searchPage li a"
    
    # GET "http://www.subtitlesource.org/" + ATTRIBUTE "href"
    # BROWSE ATTRIBUTE "href" AS FULL URL
    BROWSE "http://www.subtitlesource.org/" + ATTRIBUTE "href"
    SELECT "ul#subtitle-list li"
  
      EXTRACT SUBTITLE-DOWNLOAD_URL SELECT-ATTRIBUTE "a:eq(1)" href 
      EXTRACT SUBTITLE-PAGE_URL     SELECT-ATTRIBUTE "a:eq(2)" href 
      EXTRACT SUBTITLE-LANGUAGE_TITLE SELECT-ATTRIBUTE "a:eq(2)" title
      //EXTRACT SUBTITLE-LANGUAGE SELECT-ATTRIBUTE "a:eq(2)" title
END"""
  
  def main(args: Array[String]): Unit = {
    println( DSL.parseAll(DSL.attributeExpr, """ ATTRIBUTE "ficken" """) )
    
    
    val mdl = DSL(scraper1) 
    
    println( mdl )
  
  }
}