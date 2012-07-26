package moviemetase
package ui
package comp

import java.net.URL
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JComponent

object JImage {
  val OriginalWidth = None
  
  val Parallel = true
  val Blocking = false
  
  val Caching   = true
  val NoCaching = false
  
  lazy val LoadingImage: BufferedImage = ImageIO.read( App.resource("/res/img/image-loading.png") ) 
}

class JImage(val url: URL, resizeTo: Option[(Int, Int)], parLoad: Boolean = true, caching: Boolean = true) extends JComponent {
  import java.awt.Graphics
  import java.awt.RenderingHints
  
  setOpaque(true)
  
  private var img: BufferedImage = JImage.LoadingImage
  private var loading: Boolean = false
  private var loaded: Boolean  = false
  
  private def onImageLoaded(loadedImg: BufferedImage): Unit = UI run {
    loaded  = true
    loading = false
    
    img = loadedImg
    
    if (parLoad) {
      revalidate()
      repaint()
    }
  }
    
  private val loader =
    if (caching) new CachingImageLoader(url, onImageLoaded, resizeTo)
    else new ImageLoader(url, onImageLoaded, resizeTo)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    
    if (!loading && !loaded) {
      loading = true
      if (parLoad)
        loader.submit()
      else
        loader.execute()
    }
    
    val g2 = g.asInstanceOf[Graphics2D]
    g2.drawImage(img, 0, 0, null)
  }
  
  override def getPreferredSize(): Dimension =
    new Dimension(img.getWidth, img.getHeight)
}

class ImageLoader(url: URL, callback: BufferedImage => Unit, resizeTo: Option[(Int,Int)] = None) extends IOTask[Unit] with Logging {
  import java.io.InputStream
  
  val logID = "ImageLoader(" + url.toExternalForm + ")"
  
  def target = IOTask.getTargetByURL(url)
  
  // don't let ImageIO do the HTTP-handling, just use it to decode the InputStream
  private class HttpImageIO(val url: URL) extends HttpTask[BufferedImage] {
    def processResponse(is: InputStream): BufferedImage =
      ImageIO.read(is)
  }
    
  def execute(): Unit =
    try {
      val img =
        if (url.getProtocol == "http")
          new HttpImageIO(url).execute()
        else
          ImageIO.read(url)
                
      if (img == null)
        error("ImageIO returned NULL")
      else
        callback( resize(img) )
    
    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
    }


  // http://stackoverflow.com/questions/4220612/scaling-images-with-java-jai
  // http://stackoverflow.com/questions/932479/java2d-scaling-issues
  // TODO this? http://blogs.warwick.ac.uk/mmannion/entry/using_subsample_averaging/
  // TODO or that? https://github.com/thebuzzmedia/imgscalr
  def resize(img: BufferedImage): BufferedImage = resizeTo match {
    case None => img
    case Some(targetSize) => {
      val actualSize = (img.getWidth, img.getHeight)
      val (w, h) = calcSize(actualSize, targetSize)
      //trace("resizing from "+actualSize+" to "+targetSize+" => "+(w,h))
      
      val dest = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      val g = dest.createGraphics()
      
      g.setRenderingHint(RenderingHints.KEY_RENDERING,       RenderingHints.VALUE_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,   RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,    RenderingHints.VALUE_ANTIALIAS_ON)
      g.drawImage(img, 0, 0, w, h, null)

      g.dispose()
      img.flush()
      
      dest
    }
  }
  
  def calcSize(actual: (Int, Int), target: (Int, Int)): (Int, Int) = {
    val (actualW, actualH) = actual
    val ratio = actualW.toDouble / actualH
    target match {
      case (w, -1) => (w, (w / ratio).toInt) 
      case (-1, h) => ((h * ratio).toInt, h)
      case size    => size
    }
  }
}

class CachingImageLoader(url: java.net.URL, callback: BufferedImage => Unit, resize: Option[(Int,Int)] = None) extends IOTask[Unit] with Logging {
  val logID = "CachingImageLoader(" + url.toExternalForm + ")"
  
  val target = url.getHost
  
  val cache = new ImageCache
  lazy val loader = new ImageLoader(url, onImageLoaded _, resize)
  
  val key = url.toExternalForm + { resize match {
    case Some((w, -1)) => "___W"+w
    case Some((-1, h)) => "___H"+h
    case Some((w, h))  => "___W"+w+"H"+h
    case None          => ""
  }}

  def execute(): Unit =
    try {
      cache get key match {
        case Some(img) => {
          //trace("HIT")
          callback(img)
        }
        case None => {
          //trace("MISS")
          loader.execute()
        }
      }
    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
    }

  def onImageLoaded(img: BufferedImage): Unit = {
    //trace("PUT")
    cache put (key, img)
    
    callback(img)
  }
} 