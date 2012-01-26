package moviemetase
package ui

import java.net.URL
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JComponent
import java.awt.event.MouseAdapter
import scala.swing.Publisher
import java.awt.event.MouseEvent

object JImage {
  val OriginalWidth = None
  
  val Parallel = true
  val Blocking = false
  
  val Caching   = true
  val NoCaching = false
  
  lazy val PlaceholderImage: BufferedImage = ImageIO.read( App.resource("/res/image-loading.png") ) 
}

class JImage(val url: URL, resizeTo: Option[(Int, Int)], parLoad: Boolean = true, caching: Boolean = true) extends JComponent {
  import java.awt.Graphics
  import java.awt.RenderingHints
  
  setOpaque(true)
  
  private var img: BufferedImage = JImage.PlaceholderImage
  private var loading: Boolean = false
  private var loaded: Boolean  = false
  
  private def loaderCallback(loadedImg: BufferedImage): Unit = UI run {
    loaded  = true
    loading = false
    
    img = loadedImg
    
    revalidate()
    repaint()
  }
    
  private val loader =
    if (caching) new CachingImageLoader(url, loaderCallback, resizeTo)
    else new ImageLoader(url, loaderCallback, resizeTo)

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


class ImageLoader(url: java.net.URL, callback: BufferedImage => Unit, resizeTo: Option[(Int,Int)] = None) extends Task[Unit] with Logging {
  val logID = "ImageLoader(" + url.toExternalForm + ")"
  
  def execute(): Unit =
    try {
      trace("ImageIO reading ...")
      val img = ImageIO.read(url)
      
      if (img == null)
        error("ImageIO returned NULL")
      else
        callback( resize(img) )
    
    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
    }

  def resize(img: BufferedImage): BufferedImage = resizeTo match {
    case None => img
    case Some(size) => {
      val w = img.getWidth
      val h = img.getHeight
      
      val (destW, destH) = calcSize(size, w, h)
      trace("resizing from "+w+"/"+h+" to " + size + " => " + (destW,destH))
      
      val dest = new BufferedImage(destW, destH, BufferedImage.TYPE_INT_RGB)
      val g = dest.createGraphics()
      
      g.setRenderingHint(RenderingHints.KEY_RENDERING,     RenderingHints.VALUE_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,  RenderingHints.VALUE_ANTIALIAS_ON)
      g.drawImage(img, 0, 0, destW, destH, null)

      g.dispose()
      img.flush()
      
      dest
    }
  }
  
  def calcSize(target: (Int, Int), w: Int, h: Int): (Int, Int) = {
    val ratio = w.toDouble / h
    target match {
      case (w, -1) => (w, (w / ratio).toInt) 
      case (-1, h) => ((h * ratio).toInt, h)
      case size    => size
    }
  }
}

class CachingImageLoader(url: java.net.URL, callback: BufferedImage => Unit, resize: Option[(Int,Int)] = None) extends Task[Unit] with Logging {
  val logID = "CachingImageLoader(" + url.toExternalForm + ")"
  
  val loader = new ImageLoader(url, cachePutCallback _, resize)
  val cache = new ImageCache
  
  val key = url.toExternalForm + { resize match {
    case Some((w, -1)) => "___w"+w
    case Some((-1, h)) => "___h"+h
    case Some((w, h))  => "___w"+w+"h"+h
    case None          => ""
  }}

  def execute(): Unit =
    try {
      cache get key match {
        case Some(img) => {
          trace("HIT")
          callback(img)
        }
        case None => {
          trace("MISS")
          loader.execute()
        }
      }
    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
    }

  def cachePutCallback(img: BufferedImage): Unit = {
    cache put (key, img)
    callback(img)
  }
}