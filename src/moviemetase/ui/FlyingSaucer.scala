package moviemetase
package ui

import scala.swing._
import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}
import scala.xml._

import java.net.URL
import java.io.File


class FlyingSaucerScrollPane extends ScrollPane {
  override lazy val peer = new FSScrollPane with SuperMixin
}

//class FlyingSaucerPanel extends Component {
//  override lazy val peer = new XHTMLPanel with SuperMixin
//}


object FlyingSaucer {
  
  def loadTemplate(url: URL): Elem = {
    val f = new File( url.toURI )
    XML.loadFile( f )
  }
  
  def feedXML(panel: XHTMLPanel, elem: Elem): Unit = panel.setDocument( XMLUtils.toDOM(elem) )
  
}