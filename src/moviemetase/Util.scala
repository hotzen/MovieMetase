package moviemetase

object Util {
    implicit def stringUtils[A](s: String) = new StringUtils(s)
    implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
}

class StringUtils(val s: String) {
  def urlEncode  = java.net.URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

class ListUtils[A](val ls: List[A]) {

  // Tuple(occurrence, distinct-list-element)
  def packed: List[(Int,A)] = {
    def packIt(xs: List[A], ys: List[(Int,A)]): List[(Int,A)] = xs match {
      case Nil   => ys
      case x::tail => {
        val newXs = tail.remove(_ == x)
        val count = tail.count(_ == x) + 1
        val newYs = (count,x) :: ys
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