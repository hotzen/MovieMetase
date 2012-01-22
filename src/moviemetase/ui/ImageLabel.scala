package moviemetase
package ui

import javax.swing.JLabel
import javax.imageio.ImageIO
import java.net.URL
import java.awt.{Dimension, Graphics, Graphics2D, Image, Rectangle}
import java.awt.image.{RenderedImage, BufferedImage}

object ImageLabel {
  val OriginalWidth = None
  
  val Parallel = true
  val Blocking = false
  
  lazy val PlaceholderImage: BufferedImage = ImageIO.read( App.resource("/res/image-loading.png") ) 
}

// http://today.java.net/pub/a/today/2007/04/03/perils-of-image-getscaledinstance.html
class ImageLabel(url: URL, scaleWidth: Option[Int], parLoad: Boolean = ImageLabel.Parallel) extends JLabel {
  import java.awt.Graphics
  import java.awt.RenderingHints
  
  setOpaque(true)
    
  private var img: BufferedImage = ImageLabel.PlaceholderImage
  
  private val loadingTask = new Task[Unit] {
    def execute(): Unit =
      try {
        val img = ImageIO.read(url)
        if (img != null)
          UI.run {
            val that = ImageLabel.this
            that.img = img
            that.revalidate()
            that.repaint()
          }
        else
          println("ImageLabel.loadingTask NULL loaded " + url)
      } catch { case e:Exception => e.printStackTrace() }
  }
  private var alreadyLoad = false
  
  def load(): Unit = {
    alreadyLoad = true
    
    if (parLoad)
      loadingTask.submit()
    else
      loadingTask.execute()
  }
  
  override def paintComponent(g: java.awt.Graphics): Unit = {
    super.paintComponent(g)
    
    if (!alreadyLoad)
      load()
    
    val g2 = g.asInstanceOf[Graphics2D]
    val compSize = getSize()
    
    val newW = compSize.width
    val newH = scaleHeight(newW)
        
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    g2.drawImage(img, 0, 0, newW, newH, null);
  }
  
  def ratio: Float = img.getWidth.toFloat / img.getHeight
  
  def scaleHeight(w: Int): Int = (w / ratio).toInt
  
  override def getPreferredSize(): Dimension = scaleWidth match {
    case Some(w) => new Dimension(w, scaleHeight(w))
    case None    => new Dimension(img.getWidth, img.getHeight)
  }
}