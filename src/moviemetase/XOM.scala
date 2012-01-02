package moviemetase

import nu.xom._
import org.xml.sax.helpers.XMLReaderFactory

object XOM {
  
  object XPathContext {
    val None  = null
    val XHTML = new XPathContext("xhtml", "http://www.w3.org/1999/xhtml")
  }
        
  implicit def NodesToIterable(nodes: Nodes): Iterable[Node] = new Iterable[Node] {
    var i = 0
    def iterator = new Iterator[Node] {
      def hasNext = (i < nodes.size)
      def next = {
        val e = nodes.get(i)
        i = i + 1
        e
      }
      override def hasDefiniteSize = true
      override def isTraversableAgain = true
    }
  }
  
  implicit def ElementsToIterable(elems: Elements): Iterable[Element] = new Iterable[Element] {
    var i = 0
    def iterator = new Iterator[Element] {
      def hasNext = (i < elems.size)
      def next = {
        val e = elems.get(i)
        i = i + 1
        e
      }
      override def hasDefiniteSize = true
      override def isTraversableAgain = true
    }
  }

  implicit def ImplicitNode(node: Node): RichNode = RichNode(node)
  case class RichNode(node: Node) {
    def toElement(): Option[Element] = node match {
      case elem:Element => Some(elem)
      case _ => None
    }
    
    def xpath[A](xp: String, ctx: XPathContext = XPathContext.None): List[Node] = {
      val res =
        if (ctx != null)
          node.query(xp, ctx)
        else
          node.query(xp)
      
      res.toList
    }
    
    // http://www.xom.nu/faq.xhtml#d0e452
    def namespaces: List[Namespace] =
      node.query("namespace::node()").collect({ case n:Namespace => n }).toList
      
    
    def children: Iterable[Node] = new Iterable[Node] {
      val cnt = node.getChildCount
      var i = 0
      def iterator = new Iterator[Node] {
        def hasNext = (i < cnt)
        def next = {
          val n = node.getChild(i)
          i = i + 1
          n
        }
        override def hasDefiniteSize = true
        override def isTraversableAgain = true
      }
    }
    
//    @see getChildElements in nu.xom.Element
//    def childElements: Iterable[Element] = children.flatMap( _.toElement )
//    def childElementsByName(name: String): Iterable[Element]  = childElements.filter( _.getLocalName == name )
//    def childElementsByQName(qname: QName): Iterable[Element] = childElements.filter( elem => elem.getLocalName == qname.name && elem.getNamespaceURI == qname.ns )
  }

  
  implicit def ImplicitElement(elem: Element): RichElement = RichElement(elem)
  case class RichElement(elem: Element) extends RichNode(elem) {
        
    def name: String =
      elem.getLocalName
    
    def value: String =
      elem.getValue
    
    // only direct text-nodes, no further descendants
    def text: String = 
      elem.query("child::text()").map(_.getValue).mkString("")
    
    def attributes = new Iterable[Attribute] {
      val cnt = elem.getAttributeCount()
      var i = 0
      def iterator = new Iterator[Attribute] {
        def hasNext = (i < cnt)
        def next = {
          val e = elem.getAttribute(i)
          i = i + 1
          e
        }
        override def hasDefiniteSize = true
        override def isTraversableAgain = true
      }
    }
    
    def attribute(name: String): Option[String] = {
      val attr = elem.getAttribute(name)
      if (attr == null) None
      else              Some( attr.getValue )
    }
    
  } // EndOf RichElement
  
  implicit def ImplicitAttribut(attrib: Attribute): RichAttribute = RichAttribute(attrib)
  case class RichAttribute(attrib: Attribute) extends RichNode(attrib) {
    def name = attrib.getLocalName
    def value = attrib.getValue
  }
  
  
  case class QName(name: String, ns: String) {
    // get prefix of this QName valid in the context of the passed Node
    def prefix(ctx: Node): Option[String] =
      ctx.namespaces.find(x => x.getValue == ns && x.getPrefix.length > 0) match {
        case Some(x) => Some(x.getPrefix) 
        case None    => None
      }
    
    // get prefixed, qualified name "prefix:name"
    def prefixed(ctx: Node): String =
      prefix(ctx) match {
        case Some(p) => p + ":" + name
        case None    => name
      }
    
    override def toString: String = "%s{%s}".format(name, ns)
  }
}