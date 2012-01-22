package moviemetase
package ui

import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}
import scala.xml._
import java.net.{URI, URL}
import java.io.File
import org.w3c.dom.{Element => DOM_Element}
import org.xhtmlrenderer.layout.LayoutContext
import org.xhtmlrenderer.simple.extend.FormSubmissionListener
import org.xhtmlrenderer.swing._
import org.xhtmlrenderer.resource._
import org.xhtmlrenderer.extend._
import org.xhtmlrenderer.render._
import javax.imageio.ImageIO
import scala.swing.ScrollPane
import java.awt.Image
import org.xhtmlrenderer.simple.extend.XhtmlCssOnlyNamespaceHandler
import org.xhtmlrenderer.layout.SharedContext

object FlyingSaucer {
//  def loadTemplate(url: URL): Elem = {
//    val f = new File( url.toURI )
//    XML.loadFile( f )
//  }
//  def feedXML(panel: XHTMLPanel, elem: Elem): Unit = panel.setDocument( XMLUtils.toDOM(elem) )
}

class FS_Panel(uac: UserAgentCallback, nsh: NamespaceHandler) extends JFS_Panel {
  
  val ctx = new SharedContext(uac);
  setSharedContext( ctx )

  def load(node: Node): Unit = {
    val dom = XMLUtils.toDOM(node)
    setDocument(dom, "", nsh)
  }
    
  def relayout(): Unit = {
    sharedContext.flushFonts()
    relayout(null)
  }
  
  override def setLayout(l: java.awt.LayoutManager): Unit = ()
    //throw new IllegalStateException("setLayout() not allowed on FlyingSaucer-Panel")
}

class FS_ScrollPane extends ScrollPane {
  override lazy val peer = new FSScrollPane with SuperMixin
}

class FS_UserAgent extends NaiveUserAgent { // UserAgentCallback {

  // load css as resources from classpath
  override def getCSSResource(uri: String): CSSResource = {
    val is = App.resource(uri).openStream()
    new CSSResource(is)
  }
  
//  override def getXMLResource(uri: String): XMLResource = {
//    
//    // start in browser
//    if (uri.startsWith("http")) {
//      for (desktop <- UI.desktop)
//        desktop.browse( new URI(uri) )
//      
//      null
//      
//    // delegate to default
//    } else {
//      super.getXMLResource(uri)
//    }
//  }
  
//  def getCSSResource(uri: String): CSSResource = {
//      null 
//    }
//     
//    def getImageResource(uri: String): ImageResource = {
//      null
//    }
//    
//    def getXMLResource(String uri): XMLResource = {
//      null
//    }
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
    
    if (ns isImageElement elem)
      replaceImage(ctx, box, uac, elem, w, h)
    
    // nothing to replace
    else null
  }
    
  def replaceImage(ctx: LayoutContext, box: BlockBox, uac: UserAgentCallback, elem: DOM_Element, w: Int, h: Int): ReplacedElement = {
    val ns = ctx.getNamespaceHandler 
    val src = ns.getImageSourceURI(elem);
    println("FS_ReplacedElementFactory.replaceImage '" + src + "' w="+w+" h="+h)

    if (src == null)
      throw new NullPointerException("NULL img.src")
    
    val url = new URL(src)
    
    def imageLoadedCallback(img: java.awt.Image): Unit = {
      loadedImages  += (src -> img)
      loadingImages -= src

      panel.relayout()
      //comp.repaint()
    }
    
    if (src startsWith "http://") {
      loadedImages get src match {
        
        // already loaded
        case Some(img) => new ImageReplacedElement(img, w, h)

        // not loaded yet
        case None => {

          // already loading
          if (loadingImages contains src)
            new ImageReplacedElement(LoadingImg, w, h)

          // not loading yet
          else {
            loadingImages += src
            new ImageLoader(url, imageLoadedCallback).submit()
            
            new ImageReplacedElement(LoadingImg, w, h)
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
    
  def reset(): Unit = {
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

class FS_MouseListener extends LinkListener {
  override def linkClicked(panel: BasicPanel, uri: String): Unit = {
    println("clicked " + uri)
  }
}