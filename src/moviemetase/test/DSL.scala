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
  
  test("AttrExpr") {
    in = """ ATTRIBUTE "foo" + "bar" """
    res = DSL.parseAll(DSL.expr, in)
  }
   
  
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
    res = DSL.parseAll(DSL.scraper, in)
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
    res = DSL.parseAll(DSL.scraper, in)
  }
  
    
  def main(args: Array[String]): Unit = execute(color = false)
}