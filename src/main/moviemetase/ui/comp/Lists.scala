package moviemetase.ui.comp

import javax.swing._
import java.awt.{Component => JComponent}
import javax.swing.event._

case class ListLabelRenderer[A](f: (JLabel, Int, Boolean) => Unit) extends ListCellRenderer[A] {
  val lbl = new JLabel 
  def getListCellRendererComponent(list: JList[_ <: A], value: A, idx: Int, selected: Boolean, focused: Boolean): JComponent = {
    f(lbl, idx, selected)
    lbl
  }
}

case class OnListSelectedEvent(f: ListSelectionEvent => Unit) extends ListSelectionListener {
  def valueChanged(evt: ListSelectionEvent) {
    if (!evt.getValueIsAdjusting)
      f( evt )
  }
}
