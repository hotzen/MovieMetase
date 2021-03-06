package moviemetase

import java.net.URL
import java.net.URLEncoder
import scala.annotation.tailrec

object Util {
  import language.implicitConversions
  
  implicit def stringUtils(s: String) = new StringUtils(s)
  implicit def regexUtils(r: scala.util.matching.Regex) = new RegexUtils(r)
  implicit def listUtils[A](ls: List[A]) = new ListUtils[A](ls)
  implicit def urlUtils(url: java.net.URL) = new URLUtils(url) 
}

class StringUtils(val s: String) {
  def toURL: URL = new URL(s)

  def hardTrim   = """[^\w\.\:\?\!]+$""".r.replaceAllIn(s.trim, "") // kill remaining non-word characters
  def urlEncode  = URLEncoder.encode(s, "UTF-8")
  def noTags     = """<.*?>""".r.replaceAllIn(s, "")
  def noEntities = """&.+?;""".r.replaceAllIn(s, "")
}

class RegexUtils(val r: scala.util.matching.Regex) {
  def matches(s: String): Boolean = r.findFirstIn(s).isDefined
}

class ListUtils[A](val xs: List[A]) {
  
  def defaultEq(a: A, b: A): Boolean = ( a == b )
  
  def distinctWithoutCount(eq: (A,A) => Boolean = defaultEq): List[A] =
    distinctWithCount(eq).map( tpl => tpl._1 )
      
  def distinctWithCount(eq: (A,A) => Boolean = defaultEq): List[(A,Int)] =
    _distinctWithCount(xs, Nil, eq).sortWith( (a,b) => a._2 <= b._2 ) // sort descending by count

  @tailrec
  private final def _distinctWithCount(xs: List[A], res: List[(A,Int)], eq: (A,A) => Boolean): List[(A,Int)] = xs match {
    case Nil     => res
    case x::tail => {
      val count = 1 + tail.count( eq(_,x) )
      val rest  = tail.filterNot( eq(_,x) )
      _distinctWithCount(rest, (x, count) :: res, eq)
    }
  }
}

class URLUtils(val url: java.net.URL) {
  def getDomainName(levels: Int = 2): String = {
    val host = url.getHost
    val hostParts = host.split("\\.")
    if (hostParts.length < levels)
      return host
    
    val hostPartsArrRev = hostParts.reverse.toArray
    
    { for (i <- 0 until levels)
        yield hostPartsArrRev(i)
    }.reverse.mkString(".")
  }
  
  def domainName: String = getDomainName(2)
}
    

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

object OptionDialog {
  
}

case class DialogOption(id: Int, label: String) {
  def unapply(res: Int): Option[DialogOption] =
    if (id == res) Some(this)
    else           None
}