package moviemetase.ui

import java.awt.{Dimension, Graphics, Image, Rectangle}
import javax.swing.JLabel

class JImageLabel(val img: Image) extends JLabel {
  
  val imgSize = new Dimension(img.getWidth(null), img.getHeight(null))
  assert(imgSize.width > 0 && imgSize.height > 0)
  
  setPreferredSize( imgSize )
  setSize( imgSize )
  
  setLayout(null)

  override def paintComponent(g: Graphics): Unit = {
    val w = img.getWidth(null)
    val h = img.getHeight(null)
    
    val clip = g.getClipBounds()
    
    // center
    val x = (clip.width  - w) / 2
    val y = (clip.height - h) / 2
    
    g.drawImage(img, x, y, null)
  }
 
}