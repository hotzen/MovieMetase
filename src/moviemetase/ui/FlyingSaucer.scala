package moviemetase
package ui

import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}
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
import java.awt.Dimension
import java.awt.image.BufferedImage
import org.w3c.dom._

object FlyingSaucer {

}

class FS_Panel(uac: UserAgentCallback, nsh: NamespaceHandler) extends BasicPanel(uac) {
  
  var dom: Document = null
  
  def load(node: scala.xml.Node): Unit = {
    dom = XMLUtils.toDOM(node)
    setDocument(dom, "", nsh)
  }
  
  def reload() = {
    setDocument(dom, "", nsh)
  }
  
  def relayout(): Unit = {
//    sharedContext.flushFonts()
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
    
    if (ns isImageElement elem)
      replaceImage(ctx, box, uac, elem, w, h)
    
    // nothing to replace
    else null
  }
    
  def replaceImage(ctx: LayoutContext, box: BlockBox, uac: UserAgentCallback, elem: DOM_Element, w: Int, h: Int): ReplacedElement = {
    val ns = ctx.getNamespaceHandler 
    val src = ns.getImageSourceURI(elem);
    //println("FS_ReplacedElementFactory.replaceImage '" + src + "' w="+w+" h="+h)

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
            
            val resize = (w, h)
            //new ImageLoader(url, imageLoadedCallback, Some(resize)).submit()
            new CachingImageLoader(url, imageLoadedCallback, Some(resize)).submit()
            
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
    println("RESEEEEEEEEEEEET")
    
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
  
  def linkClicked(panel: BasicPanel, uri: URI, elem: Element): Unit = {
    val scheme = uri.getScheme
        
    scheme match {
      case "http" => 
        for (desktop <- UI.desktop)
          desktop.browse( uri )
          
      case "select" => {
        val id = uri.getSchemeSpecificPart
        
        val nsh = panel.getSharedContext.getNamespaceHandler
        
        findSelectable(elem, nsh) match {
          case Some(selectable) => {
            //val selected = getCssClasses(selectable, nsh) contains "selected"
            toggleCssClass(selectable, "selected", nsh)
            panel.asInstanceOf[FS_Panel].reload()
          }
          case None => println("NO SELECTABLE")
        }
      }
      
      case "file" =>
        for (desktop <- UI.desktop)
          desktop.open( new java.io.File(uri) )
      
      case "download" => {
        import scala.swing.FileChooser
        //import javax.swing.filechooser.FileFilter
        
        //XXX
        //val parts = uri.getSchemeSpecificPart()
        
//        val url = new URL( "http:" + parts )
//        
//        val baseDir = new File("/")
//        val fc = new FileChooser(baseDir)
//        //fc.fileFilter = new FileFilter
//        fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
//        fc.multiSelectionEnabled = false
//        fc.showSaveDialog(null) match {
//          case FileChooser.Result.Approve => {
//             val f = fc.selectedFile
//             val task = new DownloadTask(url, f)
//             task.submit()
//          }
//          case _ =>
//        }
        ()
      }
      
      case "display" => 

    }
  }
  
  override def onMouseUp(panel: BasicPanel, box: Box): Unit = {
    if (box == null)
      return
    
    val elem = box.getElement
    if (elem == null)
      return
    
    val nsh = panel.getSharedContext.getNamespaceHandler
    
    def find(n: Node): Option[(Element,URI)] = {
      if (n.getNodeType == Node.ELEMENT_NODE) {
        val elem = n.asInstanceOf[Element]
        nsh.getLinkUri( elem ) match {
          case null => find( n.getParentNode )
          case uri  => Some( (elem, URI.create(uri)) )
        }
      } else None
    }
      
    find(elem) match {
      case Some( (elem, uri) ) => linkClicked(panel, uri, elem)
      case None =>
    }
  }
  
  
  def findSelectable(elem: Element, nsh: NamespaceHandler): Option[Element] = {
    if (elem == null)
      return None
    
    if (getCssClasses(elem, nsh).contains("selectable"))
      return Some(elem)
    
    val selectables =
      for (node       <- elem.getChildNodes if node.getNodeType == Node.ELEMENT_NODE;
           element    =  node.asInstanceOf[Element];
           selectable <- findSelectable(element, nsh)) 
        yield selectable
    
    selectables.headOption
  }
    
  def toggleCssClass(elem: Element, clazz: String, nsh: NamespaceHandler): List[String] = {
    val classes = getCssClasses(elem, nsh)
    val newClasses =
      if (classes contains clazz)
        classes.filter(_ != clazz)
      else
        clazz :: classes
    
    elem.setAttribute("class", newClasses.mkString(" "))
    newClasses
  }
  
  
  def getCssClasses(elem: Element, nsh: NamespaceHandler): List[String] = {
    val clazz = nsh.getClass(elem)
    if (clazz == null)
      return Nil
    clazz.split(" ").toList //map(_.trim.toLowerCase).filter(_.length > 0).toList
  }
  
  implicit def TraversableNodeList(nodes: NodeList): Traversable[Node] = new Iterable[Node] {
    var i = 0
    def iterator = new Iterator[Node] {
      def hasNext = (i < nodes.getLength)
      def next = {
        val e = nodes.item(i)
        i = i + 1
        e
      }
      override def hasDefiniteSize = true
      override def isTraversableAgain = true
    }
  }
}