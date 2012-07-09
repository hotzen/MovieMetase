package moviemetase

import org.w3c.dom._

//object DOM {
//  implicit def TraversableNodeList(nodes: NodeList): Traversable[Node] = new Iterable[Node] {
//    var i = 0
//    def iterator = new Iterator[Node] {
//      def hasNext = (i < nodes.getLength)
//      def next = {
//        val e = nodes.item(i)
//        i = i + 1
//        e
//      }
//      override def hasDefiniteSize = true
//      override def isTraversableAgain = true
//    }
//  }
//}