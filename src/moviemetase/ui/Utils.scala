package moviemetase
package ui

import javax.swing.JComponent
import scala.swing.Publisher
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import scala.swing.Component
import scala.swing.event.Event
import scala.swing.event.MouseClicked

case class JSelected(comp: JComponent) extends Event
case class JUnselected(comp: JComponent) extends Event

trait JSelectable extends Publisher { self: JComponent =>
  var selected: Boolean = false
  
  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val thiz = JSelectable.this
      
      selected = !selected
      
      if (selected) {
        thiz.setBorder( UI.SelectionBorder )
        thiz publish JSelected( thiz )
      } else {
        thiz.setBorder( null )
        thiz publish JUnselected( thiz )
      }
    }
  })
}

//XXX not used yet
/*
case class Selected(comp: Component) extends Event
case class Unselected(comp: Component) extends Event

trait Selectable extends Publisher { self: Component =>
  var selected: Boolean = false
  
  listenTo(mouse.clicks)
  reactions += {
    case e:MouseClicked => {
      val thiz = Selectable.this
      
      selected = !selected
      
      if (selected) {
        thiz.border = UI.SelectionBorder
        thiz publish Selected( thiz )
      } else {
        thiz.border = null
        thiz publish Unselected( thiz )
      }
    }
  }
}
*/