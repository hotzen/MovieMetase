package moviemetase
package ui

import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform
import java.awt.RenderingHints

class ImageLoader(url: java.net.URL, callback: BufferedImage => Unit, resizeTo: Option[(Int,Int)] = None) extends Task[Unit] with Logging {
  import javax.imageio.ImageIO
  
  val logID = "ImageLoader(" + url.toExternalForm + ")"
  
  def execute(): Unit =
    try {
      trace("ImageIO.read() ...")
      val img = ImageIO.read(url)
      
      if (img == null) {
        error("ImageIO returned NULL")
        return
      }
      
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

  def calcSize(hint: (Int, Int), w: Int, h: Int): (Int, Int) = {
    val ratio = w.toDouble / h
    hint match {
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
    case Some((w, -1)) => "_w"+w
    case Some((-1, h)) => "_h"+h
    case Some((w, h))  => "_w"+w+"h"+h
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