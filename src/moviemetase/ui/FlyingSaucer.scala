package moviemetase
package ui

import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}
import java.net.{URI, URL}
import java.io.File
import org.w3c.dom.{Element => DOM_Element, Text => DOM_Text}
import org.xhtmlrenderer.layout.LayoutContext
import org.xhtmlrenderer.simple.extend.FormSubmissionListener
import org.xhtmlrenderer.swing._
import org.xhtmlrenderer.resource._
import org.xhtmlrenderer.extend._
import org.xhtmlrenderer.render._
import javax.imageio.ImageIO
import scala.swing.ScrollPane
import java.awt.Image
import java.awt.Dimension
import java.awt.image.BufferedImage
import org.w3c.dom._
import scala.swing.Publisher
import javax.swing.JCheckBox

object FlyingSaucer {

}

class FS_Panel(uac: UserAgentCallback, nsh: NamespaceHandler) extends BasicPanel(uac) {
  var dom: Document = null
  
  def load(node: scala.xml.Node): Unit = {
    dom = XMLUtils.toDOM(node)
    setDocument(dom, "", nsh)
  }
  
  def reload() =
    setDocument(dom, "", nsh)
    
  def relayout(): Unit =
    relayout(null) // sharedContext.flushFonts()
}

class FS_ScrollPane extends ScrollPane {
  override lazy val peer = new FSScrollPane with SuperMixin
}

class FS_UserAgent extends NaiveUserAgent { // UserAgentCallback {

  // load css as resources from classpath
  override def getCSSResource(uri: String): CSSResource = {
    //println("loading css " + uri)
    val is = App.resource(uri).openStream()
    new CSSResource(is)
  }

//    def getImageResource(uri: String): ImageResource
//    def getXMLResource(String uri): XMLResource
}

object FS_ReplacedElementFactory {
  lazy val LoadingImg = ImageIO.read( App.resource("/res/image-loading.png") )
}

class FS_ReplacedElementFactory(panel: FS_Panel) extends ReplacedElementFactory {
  import FS_ReplacedElementFactory._
  
  val loadingImages = scala.collection.mutable.Set[String]()
  val loadedImages  = scala.collection.mutable.Map[String, Image]()
    
  def createReplacedElement(ctx: LayoutContext, box: BlockBox, uac: UserAgentCallback, w: Int, h: Int): ReplacedElement = {
    val ns = ctx.getNamespaceHandler
    val elem = box.getElement
    
    if (elem == null)
      return null
    
    if (ns isImageElement elem)
      replaceImage(ctx, box, uac, elem, w, h)
    else if (elem.getTagName == "input")
      replaceInput(ctx, box, uac, elem, w, h)
    else
      null
  }
    
  def replaceImage(ctx: LayoutContext, box: BlockBox, uac: UserAgentCallback, elem: DOM_Element, w: Int, h: Int): ReplacedElement = {
    val ns = ctx.getNamespaceHandler 
    
    val src = ns.getImageSourceURI(elem);
    if (src == null)
      throw new NullPointerException("NULL img.src")
    
    val url = new URL(src)
    
    def imageLoadedCallback(img: BufferedImage): Unit = UI run {
      loadedImages  += (src -> img)
      loadingImages -= src

      if (loadingImages.size == 0 || loadedImages.size % 3 == 0)
        panel.relayout()
    }
    
    if (src startsWith "http://") {
      loadedImages get src match {
        case Some(img) => // already loaded
          new ImageReplacedElement(img, w, h)

        case None => { // not loaded yet
          if (loadingImages contains src) // already loading
            new ImageReplacedElement(LoadingImg, w, h)

          else { // not loading yet
            loadingImages += src
            
            val resizeTo = (w, h)
            val loader = new CachingImageLoader(url, imageLoadedCallback, Some(resizeTo))
            loader.submit()
            
            //new ImageReplacedElement(LoadingImg, w, h)
            new ImageReplacedElement(LoadingImg, 0, 0) // don't rescale again
          }
        }
      }
    }
    
    // delegate to UAC
    else {
      val fsImg = uac.getImageResource(src).getImage()
      if (fsImg == null)
        throw new NullPointerException("could not load resource '"+src+"'")
      
      val img = fsImg.asInstanceOf[AWTFSImage].getImage
      if (img == null)
        throw new NullPointerException("loaded resource '"+src+"' is an invalid AWT-Image")
      
      new ImageReplacedElement(img, w, h)
    }
  }
  
  def replaceInput(ctx: LayoutContext, box: BlockBox, uac: UserAgentCallback, elem: DOM_Element, w: Int, h: Int): ReplacedElement = {
    val ns = ctx.getNamespaceHandler
    
    val n = ns.getAttributeValue(elem, "name")
    val v = ns.getAttributeValue(elem, "value")
    
    ns.getAttributeValue(elem, "type") match {
      case "checkbox" => {
        val label = getParentText(elem).getOrElse("")
        val sel   = (elem.getAttributeNode("checked") != null)
        
        val checkbox = new JCheckBox(label, sel)
        new SwingReplacedElement(checkbox)
      }
      case x => throw new UnsupportedOperationException("<input type="+x+" /> is unsupported")
    }
  }
  
  def getParentText(elem: DOM_Element): Option[String] = {
    val text = elem.getParentNode.getTextContent.trim
    
    println("text of " + elem.getTagName + ": " + text)
    
    if (text.length > 0)
      Some(text)
    else
      None
  }

  def reset(): Unit = {
    //println("RESEEEEEEEEEEEET")
    loadingImages.clear()
    loadedImages.clear()
  }
  
  def remove(e: DOM_Element): Unit = {
    println("remove " + e.toString)
  }
  
  def setFormSubmissionListener(l: FormSubmissionListener): Unit = {
    println("setFormSubmissionListener " + l)
  }
}

class FS_MouseListener extends DefaultFSMouseListener {
  import org.w3c.dom._
  
  def link(panel: FS_Panel, uri: URI, elem: Element): Unit = {
    val scheme = uri.getScheme

    scheme match {
      case "http" => 
        for (desktop <- UI.desktop)
          desktop.browse( uri )
     
      case "file" =>
        for (desktop <- UI.desktop)
          desktop.open( new java.io.File(uri) )

      case x => println("unknown scheme " + x)
    }
  }
  
  def select(panel: FS_Panel, id: String, elem: Element): Unit = { }
  
  override def onMouseUp(panel: BasicPanel, box: Box): Unit = {
    if (box == null)
      return
    
    val elem = box.getElement
    if (elem == null)
      return
    
    val nsh = panel.getSharedContext.getNamespaceHandler
    
    findLink(elem, nsh) match {
      case Some( (uri, elem) ) => link(panel.asInstanceOf[FS_Panel], uri, elem)
      
      case None => findSelectable(elem, nsh) match {
        case Some( (id, elem) ) => select(panel.asInstanceOf[FS_Panel], id, elem)
        case None => 
      }
    }
  }
  
  def findLink(node: Node, nsh: NamespaceHandler): Option[(URI, Element)] = {
    if (node == null || node.getNodeType != Node.ELEMENT_NODE)
      return None
    
    val elem = node.asInstanceOf[Element]
      
    nsh.getLinkUri( elem ) match {
      case null => findLink(node.getParentNode, nsh)
      case uri  => Some( (URI.create(uri), elem) )
    }
  }

  def findSelectable(node: Node, nsh: NamespaceHandler): Option[(String, Element)] = {
    if (node == null || node.getNodeType != Node.ELEMENT_NODE)
      return None
    
    val elem = node.asInstanceOf[Element]
    
    elem getAttribute "select" match {
      case null => findSelectable(node.getParentNode, nsh)
      case ""   => findSelectable(node.getParentNode, nsh)
      case id   => Some( (id,elem) )
    }
  }

  def cssClasses(elem: Element, nsh: NamespaceHandler): List[String] =
    nsh.getClass(elem) match {
      case null  => Nil
      case clazz => clazz.split(" ").toList
    }

  def toggleCssClass(elem: Element, clazz: String, nsh: NamespaceHandler): List[String] = {
    val classes = cssClasses(elem, nsh)
    val newClasses =
      if (classes contains clazz)
        classes.filter(_ != clazz)
      else
        clazz :: classes
    
    elem.setAttribute("class", newClasses.mkString(" "))
    newClasses
  }
}