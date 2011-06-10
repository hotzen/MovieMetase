package moviemetase

object Util {
    implicit def ImplicitHtmlString(s: String): HtmlString = HtmlString(s)
  
    //def removeTags(s: String) = """</?.*?>""".r.replaceAllIn(s, "")
    //def removeEntities(s: String) = """&[a-z]+;""".r.replaceAllIn(s, "")
}

case class HtmlString(val s: String) {
  def rmTags     = """</?.*?>""".r.replaceAllIn(s, "")
  def rmEntities = """&[a-z]+;""".r.replaceAllIn(s, "")
}

//object Util {
//
//  def URLEncode(s: String): String = 
//    java.net.URLEncoder.encode(s, "UTF-8")
//    
//  def removeHTML(s: String): String =
//    s
//}
//
//trait Tracer {
//  val name: String
//  def trace(s: String): Unit
//}
//
//class FileTracer(val name: String) extends Tracer {
//  import java.io._
//  val path = scala.util.Properties.userHome + "/MovieMetase-trace-" + name + ".log"
//  val out: PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(path)));
//  
//  def trace(s: String): Unit = out.write(s)
//}
//
//class ConsoleTracer(val name: String) extends Tracer {
//  def trace(s: String): Unit = println(name + ":\t" + s)
//}