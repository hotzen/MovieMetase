package moviemetase

import nu.xom._

object XOM {
    
  def NodeToElement(n: Node): Option[Element] =
    if (n.isInstanceOf[Element])
      Some(n.asInstanceOf[Element])
    else
      None 

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

  implicit def ImplicitNode(node: Node): RichNode = new RichNode(node)
  class RichNode(node: Node) {
    
    
    // XPath
//    def \(xp: String): List[Element] =
//      xpath(node, "child::"+xp, None)
//    def \(xp: String, ctx: XPathContext): List[Element] =
//      xpath(node, "child::"+xp, Some(ctx))
//    
//    def \\(xp: String): List[Element] =
//      xpath(node, "descendant::"+xp, None)  
//    def \\(xp: String, ctx: XPathContext): List[Element] =
//      xpath(node, "descendant::"+xp, Some(ctx))

    def xpath[A](xp: String, ctx: Option[XPathContext]): List[Node] = {
      val res = ctx match {
        case Some(ctx) => node.query(xp, ctx)
        case None      => node.query(xp)
      }
      res.toList
    }
    
    
    // http://www.xom.nu/faq.xhtml#d0e452
    def namespaces: List[Namespace] =
      node.query("namespace::node()").collect({ case n:Namespace => n }).toList
  }

  
  implicit def ImplicitElement(elem: Element): RichElement = new RichElement(elem)
  class RichElement(val elem: Element) extends RichNode(elem) {
    
    def text = elem.getValue
    
    def attribute(name: String): Option[String] = {
      val attr = elem.getAttribute(name)
      if (attr == null) None
      else              Some( attr.getValue )
    }
    
    def name: String = elem.getLocalName
    
    def filterChildren(p: Element => Boolean): List[Element] =
      elem.getChildElements.
        flatMap(child => if (p(child)) Some(child) else None).
        toList
    
    def findChild(p: Element => Boolean): Option[Element] = {
      for (child <- elem.getChildElements)
        if (p(child)) return Some(child)
      None
    }

  } // EndOf RichElement
}