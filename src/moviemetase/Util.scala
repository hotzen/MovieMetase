package moviemetase

import java.net.URL
import java.net.URLEncoder

object Util {
    implicit def stringUtils[A](s: String) = new StringUtils(s)
    implicit def regexUtils[A](r: scala.util.matching.Regex) = new RegexUtils(r)
    implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
    //implicit def listTupleCountUtils[A](ls: List[(A,Int)]) = new ListTupleCountUtils[A](ls)
}

class StringUtils(val s: String) {
  def toURL: URL = new URL(s)

  def hardTrim   = """[^\w\.\:\?\!]+$""".r.replaceAllIn(s.trim, "") // kill remaining non-word characters
  def urlEncode  = URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

//object RegexUtils {
//  val Whitespace = """\s""".r
//}

class RegexUtils(val r: scala.util.matching.Regex) {
  
  def matches(s: String): Boolean =
    r.findFirstIn(s).isDefined
    
}

class ListUtils[A](val xs: List[A]) {

  def defaultEq(a: A, b: A): Boolean = ( a == b )
  
  def distinctWithoutCount(eq: (A,A) => Boolean = defaultEq): List[A] =
    distinctWithCount(eq).map( tpl => tpl._1 )
      
  def distinctWithCount(eq: (A,A) => Boolean = defaultEq): List[(A,Int)] = {
    val map = new scala.collection.mutable.HashMap[A,Int]
    for (x <- xs) {
      map.get(x) match {
        case Some(count) => map += (x -> (count+1)) 
        case None        => map += (x -> 1)
      }
    }
    map.toList.sortWith( (a,b) => a._2 > b._2 )
//    def f(xs: List[A], res: List[(A,Int)]): List[(A,Int)] = xs match {
//      case Nil => res
//      case x::tail => {
//        val count = 1 + tail.count(  eq(_,x) )
//        val nextXs = tail.filterNot( eq(_,x) )
//        f(nextXs, (x,count) :: res)
//      }
//    }
//    f(ls, Nil).sortWith( (a,b) => a._2 <= b._2 ) // sort descending by count
  }
}

//class ListTupleCountUtils[A](val ls: List[(A,Int)]) {
//  //def sortByCount(): List[(A,Int)] = ls.sortWith( (a,b) => a._2 <= b._2 )
//  //def noCount(): List[A] = ls.map(tpl => tpl._1)
//}
//      


// http://stackoverflow.com/a/4186090
object JsonType {
  class CC[T] {
    def unapply(a: Any): Option[T] = Some( a.asInstanceOf[T] )
  }

  object M extends CC[Map[String, Any]]
  object L extends CC[List[Any]]
  object S extends CC[String]
  object D extends CC[Double]
  object B extends CC[Boolean]
}