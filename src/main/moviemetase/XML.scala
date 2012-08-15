package moviemetase

import scala.xml._

object XMLUtils {
  
  val docBuilder = javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder
  val domImpl    = docBuilder.getDOMImplementation()
  
  def createDocType(qname: String, publicID: String, systemID: String): org.w3c.dom.DocumentType =
    domImpl.createDocumentType(qname, publicID, systemID)
    //domImpl.createDocumentType("html", "-//W3C//DTD XHTML 1.1//EN", "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd");
  
  def toDOM(n: Node, ns: String, qname: String, docType: org.w3c.dom.DocumentType): org.w3c.dom.Document =
    toDOM(n, domImpl.createDocument(ns, qname, docType))
  
  def toDOM(n: Node): org.w3c.dom.Document =
    toDOM(n, domImpl.createDocument(null, null, null))

  // http://stackoverflow.com/questions/2002685/any-conversion-from-scalas-xml-to-w3c-dom/2004924#2004924
  def toDOM(n: Node, doc: org.w3c.dom.Document): org.w3c.dom.Document = {
    def build(node: Node, parent: org.w3c.dom.Node): Unit = {
      val jnode: org.w3c.dom.Node = node match {
        case e: Elem => {
          val jn = doc.createElement(e.label)
          e.attributes foreach { a => jn.setAttribute(a.key, a.value.mkString) }
          jn
        }
        case a: Atom[_] => doc.createTextNode(a.text)
        case c: Comment => doc.createComment(c.commentText)
        case er: EntityRef => doc.createEntityReference(er.entityName)
        case pi: ProcInstr => doc.createProcessingInstruction(pi.target, pi.proctext)
      }
      parent.appendChild(jnode)
      node.child map { build(_, jnode) }
    }
    
    build(n, doc)
    doc
  }
}