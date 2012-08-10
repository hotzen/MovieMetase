package moviemetase
package test

import scraping._

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack

object DSL_Test extends FunSuite with BeforeAndAfter {
  
  var in: String = ""
  var res: DSL.ParseResult[_] = null
  
  before {
    in  = ""
    res = null
  }
  
  after {
    assert(!in.isEmpty)
    assert(res != null)
    
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
  
  test("SubstrExpr1") {
    in = """ "foo"(1-2) """
    res = DSL.parseAll(DSL.substrExpr, in)
  }
  
  test("SubstrExpr2") {
    in = """ "foo"(1,2) """
    res = DSL.parseAll(DSL.substrExpr, in)
  }
    
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
  
  test("SelAttrExpr") {
    in = """ SELECT-ATTRIBUTE "a:eq(1)" "href" """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedSelAttrExpr1") {
    in = """ SELECT-ATTRIBUTE "a:eq(1)" href """
    res = DSL.parseAll(DSL.expr, in)
  }
  
  test("UnquotedSelAttrExpr2") {
    in = """ SELECT-ATTRIBUTE a href """
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
  
  test("Complete Scrape") {
    in = """
SCRAPE SUBTITLES AT "SubtitleSource.org" BY "fizzl@foo"
 
  BROWSE "http://www.subtitlesource.org/search/" + <QUERY> 
  SELECT "#searchPage li a"

    BROWSE "http://www.subtitlesource.org/" + ATTRIBUTE "href"
    SELECT "ul#subtitle-list li"
  
      EXTRACT SUBTITLE-DOWNLOAD_URL SELECT-ATTRIBUTE "a:eq(1)" "href"
      EXTRACT SUBTITLE-PAGE_URL     SELECT-ATTRIBUTE "a:eq(2)" "href"
      EXTRACT SUBTITLE-LANGUAGE_TITLE SELECT-ATTRIBUTE "a:eq(2)" "title"
END"""
    res = DSL(in)
  }
  
  test("Complex Scrape") {
    in = """
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

END"""
    res = DSL(in)
  }
  
    
  def main(args: Array[String]): Unit = execute(color = false)
}