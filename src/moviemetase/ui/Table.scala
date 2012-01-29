package moviemetase.ui

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.awt.Dimension
import javax.swing.table._

case class TableCol(label: String, width: Int) {
  def clazz: Class[_] = classOf[String]
  def editable: Boolean = false
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

class TableModel[A](cols0: Traversable[TableCol]) extends ArrayTableModel[A](cols0.size) {
  
  val cols = cols0.toArray

  override def getColumnName(col: Int) = cols(col).label
  override def getColumnClass(col: Int): Class[_] = cols(col).clazz
  override def isCellEditable(row: Int, col: Int): Boolean = cols(col).editable
  
  def clear(): this.type = {
    rows.clear()
    fireTableDataChanged()
    this
  }
    
  def add(row: A): this.type = {
    rows += row
    fireTableRowsInserted(rows.length-1, rows.length-1)
    this
  }
  
  def remove(f: A => Boolean): this.type = {
    for ((row,idx) <- rows.zipWithIndex if f(row)) {
      rows remove idx
      fireTableRowsDeleted(idx, idx)
    }
    this
  }
  
  def registerColumnSettingsWith(table: Table): Unit = {
    for ( (col,idx) <- cols.zipWithIndex)
      if (col.width > 0)
        table.peer.getColumnModel.getColumn(idx).setPreferredWidth( col.width )
    ()
  }
}


class ArrayTableModel[A](colsCount: Int = 1) extends AbstractTableModel {
  val rows = new ArrayBuffer[A]
  
  def getRowCount(): Int    = rows.length
  def getColumnCount(): Int = colsCount
  
  def getValueAt(row: Int, col: Int): AnyRef = rows(row).asInstanceOf[AnyRef]
}

//class TableRenderer[A](comp: TableRendererComp[A]) extends Table.AbstractRenderer[A,TableRendererComp[A]](comp) {
//  def configure(t: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
//    component.render(a, sel, foc)
//}
//
//trait TableRendererComp[A] extends Component {
//  def render(a: A, sel: Boolean, foc: Boolean): Unit
//}

class LabelRenderer[A](f: (Label, A) => Unit) extends Table.AbstractRenderer[A, Label](new Label) {
  def configure(table: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
    f(component, a)
}

class MigPanelRenderer[A](f: (MigPanel, A, Boolean, Boolean) => Unit, layoutConstr: String = "", colConstr: String = "", rowConstr: String = "") extends Table.AbstractRenderer[A, MigPanel](new MigPanel(layoutConstr, colConstr, rowConstr)) {
  def configure(table: Table, sel: Boolean, foc: Boolean, a: A, row: Int, col: Int): Unit =
    f(component, a, sel, foc)
}