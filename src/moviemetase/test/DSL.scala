package moviemetase
package test

import scraping._
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack

object DSL_Test extends FunSuite {
  
  Logging.level = LogLevel.Trace 

  
  // ############################################
  // Basic
  
  test("QuotedQuoted") {
    val in = """ "foo" "bar" """
    DSL.parseAll(DSL.value ~ DSL.value, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("QuotedUnquoted") {
    val in = """ "foo" bar """
    DSL.parseAll(DSL.value ~ DSL.value, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedUnquoted") {
    val in = """ foo bar """
    DSL.parseAll(DSL.value ~ DSL.value, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedQuoted") {
    val in = """ foo "bar" """
    DSL.parseAll(DSL.value ~ DSL.value, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
    
  
  // ############################################
  // Expressions
  
  test("QuotedQuotedConcatExpr") {
    val in = """ "foo" + "bar" """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:ConcatExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("QuotedUnquotedConcatExpr") {
    val in = """ "foo" + bar """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:ConcatExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedQuotedConcatExpr") {
    val in = """ foo + "bar" """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:ConcatExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("ParensExpr1") {
    val in = """ ( foo ) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
  test("ParensExpr1'") {
    val in = """ (foo) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(_, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("ParensExpr2") {
    val in = """ ( foo + bar ) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(_,_), _) =>
      case res => fail(res.toString)
    }
  }
  test("ParensExpr2'") {
    val in = """ (foo + bar) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(_,_), _) =>
      case res => fail(res.toString)
    }
  }
  test("ParensExpr2''") {
    val in = """ (foo+bar) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(_,_), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("ParensExpr3") {
    val in = """ ( foo + bar ) + baz """
    DSL.parseAll(DSL.expr, in) match {
      case res@DSL.Success(ConcatExpr(ConcatExpr(_,_), _), _) => 
      case res => fail(res.toString)
    }
  }
  test("ParensExpr3'") {
    val in = """(foo + bar)+baz """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(ConcatExpr(_,_), _), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("ParensExpr4") {
    val in = """ foo + ( bar + baz ) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(_, ConcatExpr(_,_)), _) =>
      case res => fail(res.toString)
    }
  }
  test("ParensExpr4'") {
    val in = """ foo+(bar+baz) """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(_, ConcatExpr(_, _)), _) =>
      case res => fail(res.toString)
    }
  }
    
  test("SubstrExpr1") {
    val in = """ "foo"[1] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SubstrEndExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SubstrExpr2") {
    val in = """ "foo"[1-2] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SubstrPosExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SubstrExpr3") {
    val in = """ "foo"[1,2] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SubstrLenExpr, _) =>
      case res => fail(res.toString)
    }
  }
    
  test("SubstrExpr4") {
    val in = """ "foo"[-7,2] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SubstrLenExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SubstrExpr5") {
    val in = """ "foo" + bar [1,2] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ConcatExpr(e1, e2:SubstrExpr), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SubstrExpr6") {
    val in = """ ("foo" + bar) [1,2] """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SubstrExpr, _) =>
      case res => fail(res.toString)
    }
  }
      
  test("SelExpr") {
    val in = """ SELECT "a:eq(1)" """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SelExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedSelExpr") {
    val in = """ SELECT a """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SelExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("AttrExpr") {
    val in = """ ATTRIBUTE "href" """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:AttrExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedAttrExpr") {
    val in = """ ATTRIBUTE href """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:AttrExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SelAttrExpr") {
    val in = """ SELECT "a:eq(1)" ATTRIBUTE "href" """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SelAttrExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedSelAttrExpr1") {
    val in = """ SELECT "a:eq(1)" ATTRIBUTE href """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SelAttrExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedSelAttrExpr2") {
    val in = """ SELECT a ATTRIBUTE href """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(e:SelAttrExpr, _) =>
      case res => fail(res.toString)
    }
  }
  
  
  // ############################################
  // Steps
   
  test("SelectStep") {
    val in = """ SELECT "h1" END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:SelectStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedSelectStep") {
    val in = """ SELECT h1 END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:SelectStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SelectMaxStep") {
    val in = """ SELECT "h1" MAX 10 END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:SelectStep[_], _) =>
      case res => fail(res.toString)
    }
  }
    
  test("BrowseSelectStep") {
    val in = """ BROWSE SELECT "h1" + "suffix" END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BrowseStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("BrowseAttributeUnquotedStep") {
    val in = """ BROWSE ATTRIBUTE foo + bar END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BrowseStep[_], _) =>
      case res => fail(res.toString)
    }
  }

  test("ExtractStep") {
    val in = """ EXTRACT Something SELECT "div" + "suffix" END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:ExtractStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("UnquotedExtractStep") {
    val in = """ EXTRACT Something SELECT h1 + suffix END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:ExtractStep[_], _) =>
      case res => fail(res.toString)
    }
  }     
  
  
  // ############################################
  // Scrapes
  
  test("GenericScraper") {
        val in = """
SCRAPE SUBTITLES ON "FoobarSiteDescription"
  BROWSE "http://www.site.net/search/" + <QUERY>
  SELECT "ul li"
    BROWSE "http://www.site.net/" + ATTRIBUTE "attr"
    SELECT "a"
      EXTRACT Something SELECT "a:eq(1)" ATTRIBUTE "-funky-attribute"
      EXTRACT AnotherProperty SELECT "a:eq(2)" ATTRIBUTE href 
      EXTRACT PropClazz-PropName SELECT "a:eq(2)"
END"""
    DSL(in) match {
      case DSL.Success(((scraper:SubtitleScraper) :: xs), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("SubtitleSourceScraper") {
    val in = """
SCRAPE SUBTITLES ON "SubtitleSource"
  BROWSE "http://www.subtitlesource.org/search/" + <QUERY>
  SELECT "#searchPage li a"
    # store ID: last 9 chars of link
    STORE $ID ATTRIBUTE "href"[-9]
    
    # use ajax page
    BROWSE "http://www.subtitlesource.org/releaselist/" + $ID + "%7CDESC%7CAll%7CAll" WITH BASE "http://www.subtitlesource.org/title/tt1234567/"
    SELECT "#subtitle-container"
      EXTRACT Subtitle-Label SELECT "a:eq(0)"
      SELECT "#subtitle-list li"
        EXTRACT Subtitle-DownloadURL   SELECT "a:eq(0)" ATTRIBUTE href AS URL 
        EXTRACT Subtitle-PageURL       SELECT "a:eq(1)" ATTRIBUTE href AS URL
        EXTRACT Subtitle-Language      SELECT "a:eq(1)" ATTRIBUTE title
END"""
    DSL(in) match {
      case DSL.Success(((scraper:SubtitleScraper) :: xs), _) =>
      case res => fail(res.toString)
    }
  }
    
  def main(args: Array[String]): Unit = execute(color = false)
}