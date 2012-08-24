package moviemetase
package test

import extraction._
import org.scalatest.FunSuite

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
  // Selector
  
  test("BasicSelector") {
    val in = """ a.href """
    DSL.parseAll(DSL.selector, in) match {
      case DSL.Success(Selector("a.href", None, None), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("IdxSelector") {
    val in = """ a.href #1 """
    DSL.parseAll(DSL.selector, in) match {
      case DSL.Success(Selector("a.href", Some(1), None), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("MaxSelector") {
    val in = """ a.href MAX 3 """
    DSL.parseAll(DSL.selector, in) match {
      case DSL.Success(Selector("a.href", None, Some(3)), _) =>
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
  
  test("ParamExpr") {
    val in = """ <FOO> """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ParamExpr(_, None), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("ParamDefaultExpr") {
    val in = """ <FOO> DEFAULT bar """
    DSL.parseAll(DSL.expr, in) match {
      case DSL.Success(ParamExpr(_, Some(_)), _) =>
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
  
  test("BindVarStep") {
    val in = """ SET $FOO = bar END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BindVarStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("BindVarStep'") {
    val in = """ SET $FOO=bar END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BindVarStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("BindVarFromParamStep") {
    val in = """ SET $FOO = <PARAM> END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BindVarStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  test("BindVarFromParamDefaultStep") {
    val in = """ SET $FOO = <PARAM> DEFAULT baz END """
    DSL.parseAll(DSL.step, in) match {
      case DSL.Success(s:BindVarStep[_], _) =>
      case res => fail(res.toString)
    }
  }
  
  
  // ############################################
  // Extractors
  
  test("GenericSubtitlesQueryExtractor") {
        val in = """
EXTRACT SUBTITLES FROM domain.tld BY <QUERY_PARAM> ID "ExtractorID"
  BROWSE <PAGE>
  SELECT "ul li"
    BROWSE "http://www.site.net/" + ATTRIBUTE "attr"
    SELECT "a"
      EXTRACT Something SELECT "a:eq(1)" ATTRIBUTE "-funky-attribute"
      EXTRACT AnotherProperty SELECT "a:eq(2)" ATTRIBUTE href 
      EXTRACT PropClazz-PropName SELECT "a:eq(2)"
END"""
    DSL(in) match {
      case DSL.Success(((_:Extractor[_]) :: xs), _) =>
      case res => fail(res.toString)
    }
  }
  
  test("GenericSubtitlesPageExtractor") {
        val in = """
EXTRACT SUBTITLES FROM domain.tld ON <PAGE_URL_PARAM> ID "ExtractorID"
  BROWSE "http://www.site.net/search/" + <QUERY>
  SELECT "ul li"
    BROWSE "http://www.site.net/" + ATTRIBUTE "attr"
    SELECT "a"
      EXTRACT Something SELECT "a:eq(1)" ATTRIBUTE "-funky-attribute"
      EXTRACT AnotherProperty SELECT "a:eq(2)" ATTRIBUTE href 
      EXTRACT PropClazz-PropName SELECT "a:eq(2)"
END"""
    DSL(in) match {
      case DSL.Success(((_:Extractor[_]) :: xs), _) =>
      case res => fail(res.toString)
    }
  }
   
  
  test("SubtitleSourceSearchExtractor") {
    val in = """
EXTRACT SUBTITLES FROM subtitlesource.org BY <TERM> ID "SubtitleSourceOrgSearch"
  BROWSE "http://www.subtitlesource.org/search/" + <TERM>
  SELECT "#searchPage li a"
    -- BROWSE "http://www.subtitlesource.org/releaselist/" + ATTRIBUTE "href"[-9] + "%7CDESC%7CAll%7CAll" AS "http://www.subtitlesource.org/title/tt1234567/"

    SET $ID = ATTRIBUTE "href"[-9]
    BROWSE "http://www.subtitlesource.org/releaselist/" + $ID + "%7CDESC%7CAll%7CAll" AS "http://www.subtitlesource.org/title/tt1234567/"
    
    SELECT "#subtitle-container"
      EXTRACT Subtitle-Label SELECT "a:eq(0)"
      SELECT "#subtitle-list li"
        EXTRACT Subtitle-DownloadURL   ( SELECT "a:eq(0)" ATTRIBUTE href ) AS-URL 
        EXTRACT Subtitle-PageURL       ( SELECT "a:eq(1)" ATTRIBUTE href ) AS-URL
        EXTRACT Subtitle-LangText      SELECT "a:eq(1)" ATTRIBUTE title
END"""
    DSL(in) match {
      case res@DSL.Success(((extractor:Extractor[_]) :: xs), _) => {
        println(res)
      }
      case res => fail(res.toString)
    }
  }
  
  test("PodnapisiSearchExtractor") {
    val in = """
EXTRACT SUBTITLES FROM podnapisi.net BY <TERM> ID "PodnapisiNetSearch"
  SET $LANG = ( <PODNAPISI_LANG> DEFAULT "5,2" ) URL-ENCODED -- default: english, german
  BROWSE "http://www.podnapisi.net/en/ppodnapisi/search?sJ=" + $LANG + "&sK=" + ( <SEARCH> URL-ENCODED )
  
  SELECT "td.sort_column"
    EXTRACT Subtitle-Label SELECT "a.subtitle_page_link"
    EXTRACT Subtitle-PageURL ( SELECT "a.subtitle_page_link" ATTRIBUTE href ) AS-URL
    EXTRACT Subtitle-LangText SELECT "div.flag" ATTRIBUTE alt 
    EXTRACT Subtitle-ReleaseText SELECT ".release"
END"""
    DSL(in) match {
      case res@DSL.Success(((extractor:Extractor[_]) :: xs), _) => {
        println(res)
        
        val term = "Inception.1080p.BluRay.x264-REFiNED"
        //println( searcher.search(term).mkString("\n") )
      }
      case res => fail(res.toString)
    }
  }
  
  test("PodnapisiPageExtractor") {
    val in = """
EXTRACT SUBTITLES FROM podnapisi.net ON <PAGE> ID "PodnapisiNetPage"
  BROWSE <PAGE>
  SELECT "#subtitle"
    EXTRACT Subtitle-PageURL <PAGE>
    EXTRACT Subtitle-Label SELECT "h1"
    EXTRACT Subtitle-DownloadURL SELECT a.download ATTRIBUTE href AS-URL    
  
    SELECT ".right_side"
      SELECT fieldset #2
        SELECT p FIRST
          SELECT span #2
            EXTRACT Subtitle-LangText SELECT a
          DESELECT
        DESELECT
      DESELECT
      -- or DESELECT 3
      
      TRACE SELECT fieldset #3
        EXTRACT Subtitle-ReleaseText SELECT a
END"""
    DSL(in) match {
      case res@DSL.Success(((extractor:Extractor[_]) :: xs), _) => {
        println(res)
        
        val page = "http://www.podnapisi.net/en/inception-2010-subtitles-p814512"
        val url = new java.net.URL(page)
        //println( scraper.scrapePage(url).mkString("\n") )
      }
      case res => fail(res.toString)
    }
  }
    
  def main(args: Array[String]): Unit = {
    execute(color = false)
    App.shutdown
  }
}