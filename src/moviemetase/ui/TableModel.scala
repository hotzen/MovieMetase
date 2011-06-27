package moviemetase.ui

import scala.collection.mutable.ArrayBuffer
import javax.swing.table._
import scala.swing.Table
import java.awt.Dimension


trait TableModelCol {
  def label: String
  def width: Int
  def clazz: Class[_] = classOf[String]
  def editable: Boolean = false
}

trait TableModelRow {
  def value(col: Int): AnyRef
}

object TableModel {
  def apply[A <: TableModelRow](cs: Iterable[TableModelCol] = Nil, rs: Iterable[TableModelRow] = Nil): TableModel[A] = {
    val mdl = new TableModel[A]
    mdl.cols appendAll cs
    //mdl.rows appendAll rs
    mdl
  }
  
  case class Col(label: String, width: Int) extends TableModelCol {

  }
  
  case class CheckboxCol(label: String) extends TableModelCol {
    def width = 20
    override def clazz = classOf[Boolean]
    override def editable = true
  }
}

class TableModel[A <: TableModelRow] extends AbstractTableModel {
  val cols = new ArrayBuffer[TableModelCol]()
  val rows = new ArrayBuffer[A]()
  
  def getRowCount()    = rows.length
  def getColumnCount() = cols.length
  
  override def getColumnName(col: Int) = cols(col).label
  override def getColumnClass(col: Int): Class[_] = cols(col).clazz
  override def isCellEditable(row: Int, col: Int): Boolean = cols(col).editable
    
  def getValueAt(r: Int, c: Int): AnyRef = rows(r).value(c)
    
  def clear(): this.type = {
    //val last = rows.length - 1
    rows.clear()
    //fireTableRowsDeleted(0, last)
    this
  }
    
  def add(row: A): this.type = {
    rows += row
    fireTableRowsInserted(rows.length-1, rows.length-1)
    
    this
  }
    
  def setPrefSize(table: Table, height: Int): Unit = {
    val ws = for ( (col,idx) <- cols.zipWithIndex) yield {
      table.peer.getColumnModel().getColumn(idx).setPreferredWidth( col.width )
      col.width
    }
    val w = ws.sum
    table.preferredViewportSize = new Dimension(w, height)
    ()
  }
}