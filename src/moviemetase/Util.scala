package moviemetase

import java.net.URL
import java.net.URLEncoder

object Util {
    implicit def stringUtils[A](s: String) = new StringUtils(s)
    implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
}

class StringUtils(val s: String) {
  def toURL: URL = new URL(s)
  
  def urlEncode  = URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

class ListUtils[A](val ls: List[A]) {

  def defaultEq(a: A, b: A): Boolean = a == b
  
  // Tuple(distinct-list-element, occurrence)
  def packed(eq: ((A,A) => Boolean) = defaultEq _): List[(A,Int)] = {
    def packIt(xs: List[A], ys: List[(A,Int)]): List[(A,Int)] = xs match {
      case Nil   => ys
      case x::tail => {
        val newXs = tail.remove( eq(_,x) )
        val count = tail.count(  eq(_,x) ) + 1
        val newYs = (x,count) :: ys
        packIt(newXs, newYs)
      }
    }
    packIt(ls, Nil).reverse
  }
}

//trait Tracer {
//  val name: String
//  def trace(s: => String): Unit
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


trait Logging {
  def logID:  String
  def logOut: java.io.PrintStream = System.out
  
  def logTime: String = System.currentTimeMillis().toString
  
  final def trace(s: String): Unit = log("trace", s)
  final def error(s: String): Unit = log("ERROR", s) 
  
  final def log(lbl: String, s: String): Unit = {
    val sep = "\n\t" 
    logOut.println("[" + lbl + "] " + logTime + " " + logID + sep + s.grouped(100).mkString(sep))
  }
    
}