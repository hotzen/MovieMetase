package moviemetase

import java.net.URL
import java.net.URLEncoder

object Util {
    implicit def stringUtils[A](s: String) = new StringUtils(s)
    implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
    implicit def listTupleCountUtils[A](ls: List[(A,Int)]) = new ListTupleCountUtils[A](ls)
}

class StringUtils(val s: String) {
  def toURL: URL = new URL(s)
  
  def urlEncode  = URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

class ListUtils[A](val ls: List[A]) {

  def defaultEq(a: A, b: A): Boolean = a == b
  
  def countDistinct(eq: (A,A) => Boolean = defaultEq): List[(A,Int)] = {
    def f(xs: List[A], res: List[(A,Int)]): List[(A,Int)] = xs match {
      case Nil => res
      case x::tail => {
        val count = 1 + tail.count(  eq(_,x) )
        val newXs = tail.remove( eq(_,x) )
        f(newXs, (x,count) :: res)
      }
    }
    f(ls, Nil).reverse
  }
}

class ListTupleCountUtils[A](val ls: List[(A,Int)]) {
  def sortByCount(): List[(A,Int)] = ls.sortWith( (a,b) => a._2 <= b._2 )
  def noCount(): List[A] = ls.map(tpl => tpl._1)
}
      
  // Tuple(distinct-list-element, occurrence)
//  def packed(eq: ((A,A) => Boolean) = defaultEq _): List[(A,Int)] = {
//    def packIt(xs: List[A], ys: List[(A,Int)]): List[(A,Int)] = xs match {
//      case Nil   => ys
//      case x::tail => {
//        val newXs = tail.remove( eq(_,x) )
//        val count = tail.count(  eq(_,x) ) + 1
//        val newYs = (x,count) :: ys
//        packIt(newXs, newYs)
//      }
//    }
//    packIt(ls, Nil).reverse
//  }
//}

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

case class LogLevel(id: Int, label: String)
object LogLevel {
  val Trace = LogLevel(1, "trace")
  val Warn  = LogLevel(2, "warn ")
  val Error = LogLevel(3, "ERROR")
}

trait Logging {
  def logID:  String
  var logOut: java.io.PrintStream = System.out
  var minLogLevel = LogLevel.Trace
  
  
  def logTime: String = System.currentTimeMillis().toString
  
  val LOG_LINE_MAX_LEN = 300
  val LOG_LINE_SEP = "\n  "
  
  final def trace(msg: String, ps: List[(String,Any)] = Nil): Unit = log(LogLevel.Trace, msg, ps)
  final def warn(msg: String,  ps: List[(String,Any)] = Nil): Unit = log(LogLevel.Warn,  msg, ps)
  final def error(msg: String, ps: List[(String,Any)] = Nil): Unit = log(LogLevel.Error, msg, ps) 
      
  final def log(lvl: LogLevel, msg: String, ps: List[(String,Any)]): Unit = {
    if (lvl.id >= minLogLevel.id) {
      
      val s =
        if (ps.isEmpty) msg
        else            msg + " {" + ps.map( p => p._1+"="+p._2 ).mkString("; ") + "}"
      
      logOut.println("[" + lvl.label + "] " + logTime + " " + logID + "\t" + s.grouped(LOG_LINE_MAX_LEN).mkString(LOG_LINE_SEP))
    }
  }
}