package moviemetase

import java.net.URL
import java.net.URLEncoder

object Util {
    implicit def stringUtils[A](s: String) = new StringUtils(s)
    implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
    //implicit def listTupleCountUtils[A](ls: List[(A,Int)]) = new ListTupleCountUtils[A](ls)
}

class StringUtils(val s: String) {
  def toURL: URL = new URL(s)

  def urlEncode  = URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

class ListUtils[A](val ls: List[A]) {

  def defaultEq(a: A, b: A): Boolean = ( a == b )
  
  def distinctNoCount(eq: (A,A) => Boolean = defaultEq): List[A] =
    distinctCount(eq).map( tpl => tpl._1 )
  
  def distinctCount(eq: (A,A) => Boolean = defaultEq): List[(A,Int)] = {
    def f(xs: List[A], res: List[(A,Int)]): List[(A,Int)] = xs match {
      case Nil => res
      case x::tail => {
        val count = 1 + tail.count(  eq(_,x) )
        val nextXs = tail.filterNot( eq(_,x) )
        f(nextXs, (x,count) :: res)
      }
    }
    f(ls, Nil).sortWith( (a,b) => a._2 <= b._2 ) // sort descending by count
  }
}

//class ListTupleCountUtils[A](val ls: List[(A,Int)]) {
//  //def sortByCount(): List[(A,Int)] = ls.sortWith( (a,b) => a._2 <= b._2 )
//  //def noCount(): List[A] = ls.map(tpl => tpl._1)
//}
//      
