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
  private var _selected: Boolean = false
  
  def selected: Boolean = _selected

  def select(): Unit = {
    _selected = true
    this setBorder UI.SelectionBorder
    this publish JSelected( this )
  }

  def unselect(): Unit = {
    _selected = false
    this setBorder null
    this publish JUnselected( this )
  }
  
  def select(sel: Boolean): Unit =
    if (sel)
      select()
    else
      unselect()

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = select( !_selected )
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