package moviemetase

import org.jsoup._
import org.jsoup.nodes._
import org.jsoup.select._

object JSoup {
  
  implicit def _IterableElements(elems: Elements): Iterable[Element] =
    scala.collection.JavaConversions.JIteratorWrapper( elems.iterator() ).toIterable

  implicit def _IterableAttributes(attrs: Attributes): Iterable[Attribute] =
    scala.collection.JavaConversions.JIteratorWrapper( attrs.iterator() ).toIterable
    
  implicit def _RichElement(elem: Element): RichElement = new RichElement(elem)
  
  class RichElement(elem: Element) {
    def attrOpt(name: String): Option[String] = elem.attr(name) match {
      case "" => None
      case a  => Some(a)
    }
  }
}