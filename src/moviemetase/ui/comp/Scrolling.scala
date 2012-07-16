package moviemetase.ui.comp

import javax.swing.JPanel
import javax.swing.{Scrollable => JScrollable}
import java.awt.Dimension
import java.awt.Rectangle
import scala.swing.SequentialContainer
import scala.swing.Scrollable
import scala.swing.Panel
import javax.swing.{Scrollable => JScrollable}

class ScrollablePanel extends Panel with SequentialContainer.Wrapper with Scrollable.Wrapper {
  
  var scrollIncrement: Int = 10
  var blockScrollIncrement: Int = 50
  
  val allowVerticalScrolling: Boolean   = true
  val allowHorizontalScrolling: Boolean = false
  
  override lazy val peer = new JPanel with SuperMixin with JScrollable {
    def getPreferredScrollableViewportSize: Dimension =
      getPreferredSize
  
    def getScrollableTracksViewportHeight: Boolean =
      !allowVerticalScrolling
    
    def getScrollableTracksViewportWidth: Boolean =
      !allowHorizontalScrolling
    
    def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int =
      scrollIncrement
    
    def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int =
      blockScrollIncrement
  }
  
  final protected def scrollablePeer: JScrollable = peer
}

