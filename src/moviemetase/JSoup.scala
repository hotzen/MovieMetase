package moviemetase

import org.jsoup._
import org.jsoup.nodes._
import org.jsoup.select._

object JSoup {
  import language.implicitConversions
  
  implicit def _IterableElements(elems: Elements): Iterable[Element] =
    scala.collection.convert.Wrappers.JIteratorWrapper[Element]( elems.iterator() ).toIterable

  implicit def _IterableAttributes(attrs: Attributes): Iterable[Attribute] =
    scala.collection.convert.Wrappers.JIteratorWrapper[Attribute]( attrs.iterator() ).toIterable
    
  implicit def _RichElement(elem: Element): RichElement = new RichElement(elem)
  
  class RichElement(elem: Element) {
    def attrOpt(name: String): Option[String] = elem.attr(name) match {
      case "" => None
      case a  => Some(a)
    }
  }
}