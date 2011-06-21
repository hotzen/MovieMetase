package moviemetase.ui

import scala.collection.mutable.ArrayBuffer
import javax.swing.table._
import scala.swing.Table


trait TableModelCol {
  def label: String
  def width: Option[Int]
}

trait TableModelRow {
  def value(col: Int): AnyRef
}

object TableModel {
  def apply(cs: Iterable[TableModelCol] = Nil, rs: Iterable[TableModelRow] = Nil): TableModel = {
    val mdl = new TableModel
    mdl.cols appendAll cs
    mdl.rows appendAll rs
    mdl
  }
  
  case class Col(label: String, width: Option[Int] = None) extends TableModelCol {

  }
}

class TableModel extends AbstractTableModel {
  val cols = new ArrayBuffer[TableModelCol]()
  val rows = new ArrayBuffer[TableModelRow]()
  
  def getRowCount()    = rows.length
  def getColumnCount() = cols.length
  
  override def getColumnName(col: Int) = cols(col).label
            
  def getValueAt(r: Int, c: Int): AnyRef = rows(r).value(c)
  
  def addCol(col: TableModelCol): this.type = {
    cols += col
    this
  }
    
  def addRow(row: TableModelRow): this.type = {
    rows += row
    fireTableRowsInserted(rows.length-1, rows.length-1)
    
    this
  }
  
  
  def registerColWidth(table: Table): Unit =
    for ( (col,idx) <- cols.zipWithIndex if !col.width.isEmpty) 
      table.peer.getColumnModel().getColumn(idx).setPreferredWidth( col.width.head )
}