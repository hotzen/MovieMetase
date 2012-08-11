package moviemetase
package test

import scraping._

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack

object DSL_Test extends FunSuite with BeforeAndAfter {
  
  Logging.level = LogLevel.Trace 
  
  var in: String = ""
  var res: DSL.ParseResult[_] = null
  
  before {
    in  = ""
    res = null
  }
  
  after {
    assert(in != null && !in.isEmpty)
    assert(res != null, "ParseResult is NULL")
    
    assert(!res.isEmpty, "\"\"\"" + in + "\"\"\" => " + res.toString)
  }
  
  
  // ############################################
  // Basic
  
  test("ValueValueSeqExpr1") {
    in = """ "foo" "bar" """
    res = DSL.parseAll(DSL.value ~ DSL.value, in)
  }
  
  test("ValueValueSeqExpr2") {
    in = """ "foo" bar """
    res = DSL.parseAll(DSL.value ~ DSL.value, in)
  }
  
  test("ValueValueSeqExpr3") {
    in = """ foo bar """
    res = DSL.parseAll(DSL.value ~ DSL.value, in)
  }
  
  test("ValueValueSeqExpr4") {
    in = """ foo "bar" """
    res = DSL.parseAll(DSL.value ~ DSL.value, in)
  }
  
  
  // ############################################
  // Expressions
  
  test("ConcatExpr") {
    in = """ "foo" + "bar" """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedConcatExpr") {
    in = """ "foo" + bar """
    res = DSL.parseAll(DSL.expr, in)
  }
  
//  test("SubstrExpr1") {
//    in = """ "foo"(1) """
//    res = DSL.parseAll(DSL.expr, in)
//  }
//  
//  test("SubstrExpr2") {
//    in = """ "foo"(1-2) """
//    res = DSL.parseAll(DSL.expr, in)
//  }
//  
//  test("SubstrExpr3") {
//    in = """ "foo"(1,2) """
//    res = DSL.parseAll(DSL.expr, in)
//  }
//  
//  test("SubstrExpr4") {
//    in = """ "foo"(-7,2) """
//    res = DSL.parseAll(DSL.expr, in)
//  }
    
  test("SelExpr") {
    in = """ SELECT "a:eq(1)" """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedSelExpr") {
    in = """ SELECT a """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("AttrExpr") {
    in = """ ATTRIBUTE "href" """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedAttrExpr") {
    in = """ ATTRIBUTE href """
    res = DSL.parseAll(DSL.expr, in)
  }
  
//  test("SelAttrExpr") {
//    in = """ SELECT-ATTRIBUTE "a:eq(1)" "href" """
//    res = DSL.parseAll(DSL.expr, in)
//  }
//  
//  test("UnquotedSelAttrExpr1") {
//    in = """ SELECT-ATTRIBUTE "a:eq(1)" href """
//    res = DSL.parseAll(DSL.expr, in)
//  }
//  
//  test("UnquotedSelAttrExpr2") {
//    in = """ SELECT-ATTRIBUTE a href """
//    res = DSL.parseAll(DSL.expr, in)
//  }
  
  test("SelAttrExpr") {
    in = """ SELECT "a:eq(1)" ATTRIBUTE "href" """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedSelAttrExpr1") {
    in = """ SELECT "a:eq(1)" ATTRIBUTE href """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedSelAttrExpr2") {
    in = """ SELECT a ATTRIBUTE href """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  
  // ############################################
  // Steps
   
  test("SelectStep") {
    in = """ SELECT "h1" END """
    res = DSL.parseAll(DSL.step, in)
  }
  
  test("UnquotedSelectStep") {
    in = """ SELECT h1 END """
    res = DSL.parseAll(DSL.step, in)
  }
  
  test("BrowseStep") {
    in = """ BROWSE SELECT "h1" + "suffix" END """
    res = DSL.parseAll(DSL.step, in)
  }
  
  test("SelectMaxStep") {
    in = """ SELECT "h1" MAX 10 END """
    res = DSL.parseAll(DSL.step, in)
  }
  
  test("UnquotedBrowseStep") {
    in = """ BROWSE ATTRIBUTE foo + bar END """
    res = DSL.parseAll(DSL.step, in)
  }

  test("ExtractStep") {
    in = """ EXTRACT Something SELECT "div" + "suffix" END """
    res = DSL.parseAll(DSL.step, in)
  }
  
  test("UnquotedExtractStep") {
    in = """ EXTRACT Something SELECT h1 + suffix END """
    res = DSL.parseAll(DSL.step, in)
  }     
  
  
  // ############################################
  // Scrapes
  
  test("SubtitleScrape") {
        in = """
SCRAPE SUBTITLES ON "FoobarSiteDescription"
  BROWSE "http://www.site.net/search/" + <QUERY>
  SELECT "ul li"
    BROWSE "http://www.site.net/" + ATTRIBUTE "attr"
    SELECT "a"
      EXTRACT Something SELECT "a:eq(1)" ATTRIBUTE "-funky-attribute"
      EXTRACT AnotherProperty SELECT "a:eq(2)" ATTRIBUTE href 
      EXTRACT PropClazz-PropName SELECT "a:eq(2)"
END"""
    res = DSL(in)
    //println(res)
  }
  
  test("RunSubtitleScrape") {
//    in = """
//SCRAPE SUBTITLES ON "SubtitleSource"
//  BROWSE "http://www.subtitlesource.org/search/" + <QUERY>
//  SELECT "#searchPage li a"
//    BROWSE ATTRIBUTE "href"
//    SELECT "#subtitle-container"
//      EXTRACT Subtitle-Label SELECT "a:eq(1)" ATTRIBUTE href
//      SELECT "#subtitle-list li"
//        EXTRACT Subtitle-DownloadURL   SELECT "a:eq(1)" ATTRIBUTE href 
//        EXTRACT Subtitle-PageURL       SELECT "a:eq(2)" ATTRIBUTE href 
//        EXTRACT Subtitle-LanguageLabel SELECT "a:eq(2)" ATTRIBUTE title
//END"""
    in = """
SCRAPE SUBTITLES ON "SubtitleSource"
  BROWSE "http://www.subtitlesource.org/search/" + <QUERY>
  SELECT "#searchPage li a"
    #TRACE STORE $ID ATTRIBUTE "href"
    TRACE BROWSE "http://www.subtitlesource.org/releaselist/tt1375666%7CDESC%7CAll%7CAll" WITH BASE "http://www.subtitlesource.org/title/tt1234567/"
    SELECT "#subtitle-container"
      EXTRACT Subtitle-Label SELECT "a:eq(0)"
      TRACE SELECT "#subtitle-list li"
        EXTRACT Subtitle-DownloadURL   SELECT "a:eq(0)" ATTRIBUTE href AS URL 
        EXTRACT Subtitle-PageURL       SELECT "a:eq(1)" ATTRIBUTE href AS URL
        EXTRACT Subtitle-LanguageLabel SELECT "a:eq(1)" ATTRIBUTE title
      
END"""
    res = DSL(in)
    
    import DSL._
    res match {
      case pr:ParseResult[List[SubtitleScraper]] => pr match {
        case Success(scrapers, _) => {
          for (scraper <- scrapers) {
            println(scraper)
            scraper.scrape("Inception.1080p.BluRay.x264-REFiNED")
          }
          ()
        }
      }
    }
    
  }
  
//  test("RunScrape") {
//    in = """
//SCRAPE SUBTITLES ON "SubtitleSource"
// 
//  BROWSE "http://www.subtitlesource.org/search/" + <QUERY> 
//  SELECT "#searchPage li a"
//
//    BROWSE ATTRIBUTE "href"
//    
//    SELECT "td#subtitle-container"
//      EXTRACT Subtitle-Label SELECT-ATTRIBUTE "a:eq(1)" href
//      
//    SELECT "#release-list ul#subtitle-list li"
//  
//      EXTRACT Subtitle-DownloadURL   SELECT-ATTRIBUTE "a:eq(1)" href 
//      EXTRACT Subtitle-PageURL       SELECT-ATTRIBUTE "a:eq(2)" href 
//      EXTRACT Subtitle-LanguageLabel SELECT-ATTRIBUTE "a:eq(2)" title
//
//END"""
//    res = DSL(in)
//  }
  
    
  def main(args: Array[String]): Unit = execute(color = false)
}