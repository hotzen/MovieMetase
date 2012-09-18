package moviemetase
package extraction

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.matching.Regex
import java.util.regex.Pattern
import java.net.MalformedURLException
import scala.util.parsing.combinator.lexical.Lexical

class InvalidExtractor(val msg: String) extends Exception(msg)
class InvalidExtractorType(val ty: String) extends InvalidExtractor("invalid extractor-type '" + ty + "'")

object DSL extends RegexParsers with PackratParsers {
  
  // haskell style comments: "... -- comment"
  override protected val whiteSpace = """(\s|\Q--\E.*|\#.*)+""".r
  
  // ********************************************
  // basic
  
  val quotedRegex = """"([^"]*)"""".r
  val quoted = regexMatch(quotedRegex) ^^ { case m => m.group(1) }
  val unquoted = """[^\s\Q"[]()+\E]+""".r
  val value = quoted | unquoted
  
  val int = """-?\d+""".r ^^ { d => d.toInt }
  
  val keyValuePair = value ~ "=" ~ value ^^ {
    case k ~ _ ~ v => (k, v) 
  }
  
  // ********************************************
  // selector
  
  
  val selectorIdxFirst = "FIRST" ^^^ 1
  val selectorIdxLast = "LAST" ^^^ -1
//  val selectorIdxNum = "#" ~> int ^^ {
//    case i => i.toInt
//  }
  val selectorIdx = selectorIdxFirst | selectorIdxLast // | selectorIdxNum
  
  val selectorMax = "MAX" ~> int
    
  val selector = value ~ opt(selectorIdx) ~ opt(selectorMax) ^^ {
    case s ~ idx ~ max =>
      Selector(s, idx, max) 
  }
  
  
  // ********************************************
  // expressions
  val literalExpr = value ^^ { case s => LitExpr(s) }
  
  val paramName = "<" ~> """[A-Z_]+""".r <~ ">"
  val paramDefault = "DEFAULT" ~> value
  val paramExpr = paramName ~ opt(paramDefault) ^^ {
    case name ~ default => ParamExpr(name, default)
  }
  
  val varName = """[A-Z]+""".r
  val varExprParser = "$" ~> varName
  val varExpr = varExprParser ^^ {
    case name => VarExpr(name)
  }
  
  val separatedBy = "SEPARATED BY" ~> value
  
  val selExpr = "SELECT" ~ selector ~ opt(separatedBy) ^^ {
    case _ ~ sel ~ sep =>
      SelExpr(sel, sep.getOrElse(""))
  }
  
  val attrExpr = "ATTRIBUTE" ~> value ^^ {
    case name => AttrExpr(name)
  }
  
  val selAttrExpr: PackratParser[Expr] = "SELECT" ~ selector ~ "ATTRIBUTE" ~ value ~ opt(separatedBy) ^^ {
    case _ ~ sel ~ _ ~ attr ~ sep =>
      SelAttrExpr(sel, attr, sep.getOrElse(""))
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


  // ********************************************
  // steps
  val trace = opt("TRACE") ^^ { _.isDefined }
  val asURL = "AS" ~> value
  
  val getStep: Parser[Step[_]] = trace ~ "GET" ~ expr ~ opt(asURL) ~ step ^^ {
    case t ~ _ ~ e ~ url ~ next =>
      GetStep(e, url, next).trace(t)
  } 
  
  val postData = "DATA" ~> rep1(keyValuePair)
  val postStep: Parser[Step[_]] = trace ~ "POST" ~ expr ~ opt(asURL) ~ postData ~ step ^^ {
    case t ~ _ ~ e ~ url ~ data ~ next =>
      PostStep(e, data, url, next).trace(t)
  }
  
  val selectStep: Parser[Step[_]] = trace ~ "SELECT" ~ selector ~ step ^^ {
    case t ~ _ ~ sel ~ next =>
      SelectStep(sel, next).trace(t)
  }
  
  val deselectStep: Parser[Step[_]] = trace ~ ( "DESELECT" | "UNSELECT" ) ~ opt(int) ~ step ^^ {
    case t ~ _ ~ num ~ next =>
      DeselectStep(num.getOrElse(1), next).trace(t)
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
  
  val step: Parser[Step[_]] = getStep | postStep | selectStep | deselectStep | extractStep | bindVarStep | finalStep  

  
  // ********************************************
  // extractors
  
  val extractorType = """[A-Z]+""".r
  val extractorId = "ID" ~> value
  val extractorDomain = value ^^ { v => v.toLowerCase }
  val extractorParamTy = "BY" ^^^ ExtractorParamType.Term | "ON" ^^^ ExtractorParamType.Page
  
  val extractor: Parser[Extractor[_]] = trace ~ "EXTRACT" ~ extractorType ~ "FROM" ~ extractorDomain ~ extractorParamTy ~ paramName ~ extractorId ~ step ^^ {
    case t ~ _ ~ ty ~ _ ~ dom ~ paramTy ~ paramName ~ id ~ step => {
      val extr = ty match { // XXX refactor
        case "SUBTITLES" => {
          Extractor[MovieInfos.Subtitle](id, dom, paramTy, paramName, step.asInstanceOf[Step[MovieInfos.Subtitle]], new SubtitleFactory)
        }
        case x => throw new InvalidExtractorType(x)
      }
      extr.trace(t)
    }
  }

  val extractors = rep1(extractor)
  
  
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
  
  def parse(s: String): ParseResult[List[Extractor[_]]] =
    parseAll(extractors, s)
    //phrase(extractor)(new Scanner(s))
  
  def apply(s: String): List[Extractor[_]] = parse(s) match {
    case DSL.Success(res, _)   => res
    case fail@DSL.NoSuccess(msg, _) => throw new InvalidExtractor(fail.toString)
  }
}