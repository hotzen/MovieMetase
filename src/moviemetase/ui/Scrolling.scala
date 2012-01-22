package moviemetase.ui

import javax.swing.{JComponent, SwingConstants}
import java.awt.{Dimension, Rectangle}

// http://stackoverflow.com/questions/2716274/jscrollpane-needs-to-shrink-its-width/2814718#2814718
trait OnlyVerticallyScrollable extends javax.swing.Scrollable { self: JComponent =>
    
  def getScrollableTracksViewportWidth(): Boolean  = true
  def getScrollableTracksViewportHeight(): Boolean = false
  
  def getPreferredScrollableViewportSize(): Dimension = this.getPreferredSize
  
  val ScrollIncrement = 10
  
  def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = ScrollIncrement;

  def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = {
    val incr =   
      if (orientation == SwingConstants.VERTICAL)
        visibleRect.height / 2
      else
        visibleRect.width / 2
    
    incr - ScrollIncrement
  }
}