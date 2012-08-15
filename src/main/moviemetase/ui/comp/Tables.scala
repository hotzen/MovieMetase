package moviemetase
package ui
package comp

//import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.awt.{Component => JComponent}
import javax.swing._
import javax.swing.table._
import javax.swing.event._
import java.awt.LayoutManager

case class TableCol(label: String, prefWidth: Int) {
  
  var clazz: Class[_] = classOf[String]
  var editable: Boolean = false
  var maxWidth: Int = -1
}

//object SmartTableModel {
//  def apply[A <: TSmartTableRow](cols: Iterable[TSmartTableCol] = Nil, rows: Iterable[TSmartTableRow] = Nil): SmartTableModel[A] = {
//    val mdl = new SmartTableModel[A]
//    mdl.cols appendAll cols
//    //mdl.rows appendAll rs
//    mdl
//  }
//    
  
  
//  case class ImageCol(label: String, width: Int) extends TableModelCol {
//    override val renderer: TableCellRenderer = new DefaultTableCellRenderer {
//      val lbl = new JLabel
//      override def getTableCellRendererComponent(tbl: javax.swing.JTable, value: Object, selected: Boolean, focus: Boolean, row: Int, col: Int): java.awt.Component  = {
//        //lbl.setIcon( icon )
//        lbl.setIcon( value.asInstanceOf[ImageIcon] )
//        lbl
//      }
//    }
//  }
  
//  case class CheckboxCol(label: String) extends TableModelCol {
//    val width = 20
//    override val clazz = classOf[Boolean]
//    override val editable = true
//  }
//}

abstract class TableModel[A](_cols: Traversable[TableCol]) extends AbstractTableModel {
  val cols = _cols.toArray
  val rows = new ArrayBuffer[A]

  def getRowCount(): Int    = rows.length
  def getColumnCount(): Int = cols.length
  
  def getValueAt(row: Int, col: Int): AnyRef =
    getValue(rows(row).asInstanceOf[A], col)
  
  // IMPLEMENT ME
  def getValue(row: A, col: Int): AnyRef 

  override def getColumnName(col: Int) = cols(col).label
  override def getColumnClass(col: Int): Class[_] = cols(col).clazz
  override def isCellEditable(row: Int, col: Int): Boolean = cols(col).editable

  def clear() {
    rows.clear()
    fireTableDataChanged()
  }
    
  def add(row: A) {
    rows += row
    fireTableRowsInserted(rows.length-1, rows.length-1)
  }
  
  def remove(p: A => Boolean) {
    var continue = true
    while (continue) {
      rows.zipWithIndex.find({ case (row, idx) => p(row) }) match {
        case Some((row, idx)) => {
          rows remove idx
          fireTableRowsDeleted(idx, idx)
          
          continue = true
        }
        case None =>
          continue = false
      }
    }
  }
  
  def update(f: (A,A) => Boolean)(newRow: A) {
    val updIdx = 
      for ((oldRow,idx) <- rows.zipWithIndex.toList if f(oldRow, newRow)) yield {
        rows(idx) = newRow
        fireTableRowsUpdated(idx, idx)
        idx
      }

    if (updIdx.isEmpty) {
      add(newRow)
    }
  }
}

case class TableLabelRenderer[A](f: (JLabel, A, Int, Int, Boolean) => Unit) extends TableCellRenderer {
//  val lbl = new JLabel {
//    override def invalidate() {}
//    override def validate() {}
//    override def revalidate() {}
//    override def repaint(tm: Long, x: Int, y: Int, w: Int, h: Int) {}
//    override def repaint(r: java.awt.Rectangle) { }
//    override def repaint() { }
//  }
  val lbl = new DefaultTableCellRenderer
  
  def getTableCellRendererComponent(table: JTable, value: Any, selected: Boolean, focused: Boolean, row: Int, col: Int): JComponent = {
    f(lbl, value.asInstanceOf[A], row, col, selected)
    lbl
  }
}

//case class TableMigPanelRenderer[A](f: (MigPanel, A, Int, Int, Boolean) => Unit, layoutConstr: String = "", colConstr: String = "", rowConstr: String = "") extends TableCellRenderer {
//  val pnl = new MigPanel(layoutConstr, colConstr, rowConstr) 
//  def getTableCellRendererComponent(table: JTable, value: Any, selected: Boolean, focused: Boolean, row: Int, col: Int): JComponent = {
//    f(pnl, value.asInstanceOf[A], row, col, selected)
//    pnl.peer
//  }
//}

case class TablePanelRenderer[A](f: (JPanel, A, Int, Int, Boolean) => Unit, layout: LayoutManager) extends TableCellRenderer {
  val pnl = new JPanel
  pnl setLayout layout
  def getTableCellRendererComponent(table: JTable, value: Any, selected: Boolean, focused: Boolean, row: Int, col: Int): JComponent = {
    f(pnl, value.asInstanceOf[A], row, col, selected)
    pnl
  }
}

//class MigPanelRenderer[A](f: (MigPanel, A, Boolean, Boolean) => Unit, layoutConstr: String = "", colConstr: String = "", rowConstr: String = "") extends Table.AbstractRenderer[A, MigPanel](new MigPanel(layoutConstr, colConstr, rowConstr)) {
//  def configure(table: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
//    f(component, a, sel, foc)
//}

//class TableRenderer[A](comp: TableRendererComp[A]) extends Table.AbstractRenderer[A,TableRendererComp[A]](comp) {
//  def configure(t: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
//    component.render(a, sel, foc)
//}

//trait TableRendererComp[A] extends Component {
//  def render(a: A, sel: Boolean, foc: Boolean): Unit
//}

//class LabelRenderer[A](f: (Label, A) => Unit) extends Table.AbstractRenderer[A, Label](new Label) {
//  def configure(table: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
//    f(component, a)
//}

//class MigPanelRenderer[A](f: (MigPanel, A, Boolean, Boolean) => Unit, layoutConstr: String = "", colConstr: String = "", rowConstr: String = "") extends Table.AbstractRenderer[A, MigPanel](new MigPanel(layoutConstr, colConstr, rowConstr)) {
//  def configure(table: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
//    f(component, a, sel, foc)
//}