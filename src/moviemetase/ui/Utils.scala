package moviemetase
package ui

import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform
import java.awt.RenderingHints

class ImageLoader(url: java.net.URL, callback: BufferedImage => Unit, resize: Option[(Int,Int)] = None) extends Task[Unit] with Logging {
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
      
      resize match {
        case None => UI.run { callback(img) }
        
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
          
          UI.run { callback(dest) }
        }
      }

    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
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
