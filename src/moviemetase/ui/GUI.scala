package moviemetase
package ui

import scala.swing._, Swing._
import scala.swing.event._
import javax.swing.JOptionPane

class GUI extends Reactor {
  val Title = "MovieMetase"
  
  def start(): Unit = {
    import javax.swing.UIManager
    
    // native Look&Feel
    UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() )
    javax.swing.JFrame.setDefaultLookAndFeelDecorated(true)
    
    // apple integration
    val props = System.getProperties
    props setProperty("apple.laf.useScreenMenuBar", "true")
    props setProperty("com.apple.mrj.application.apple.menu.about.name", Title)
    
    Swing.onEDT {
      Top.pack()
      Top.visible = true
    }
  }
  
  val Top = new Frame {
    title = Title
    contents = Main
    override def closeOperation = App.shutdown()
  }
  
  object Main extends MigPanel("wrap 2", "[fill][grow,fill]", "[fill][fill][min!]") {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    
    add(DropLabel)
    add(QueryPanel, "grow, span")
    
    add(StatusBar, "dock south, span")
  }
  
  object StatusBar extends MigPanel() {
    add(new Label("Fooo"))
    
  }
  
  object DropLabel extends Label {
    override lazy val peer: javax.swing.JLabel = new JImageLabel( App.image("/res/drop.png") )
        
    val dropHandler = new FileDropHandler
    listenTo(dropHandler)
    peer.setTransferHandler(dropHandler)
    
    border  = EtchedBorder
    tooltip = "DROP FILE HERE"
    
    reactions += {
      case FileDropHandler.FilesDropped(files) => {
        
        if (files.isEmpty) {
          JOptionPane.showMessageDialog(null, "Invalid File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
        } else if (!files.tail.isEmpty) {
          JOptionPane.showMessageDialog(null, "Please drop exactly 1 File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
        } else if (files.head.isDirectory) {
          JOptionPane.showMessageDialog(null, "Please drop exactly one File, no Directory", "Dropped Files", JOptionPane.ERROR_MESSAGE);
        } else {
          val file = files.head
          val fileInfo = FileInfo.create( file )
          val disFileInfo = fileInfo.dissect() 
          
//          val s = new MovieSearch
//          s.search( fileInfo )
          
          //val queries = MovieQueryGenerator generateFrom movieFile

//          for ( (q,i) <- queries.zipWithIndex) {
//            QueryPanel.tblModel.addRow(
//              QueryPanel.QueryRow(i, q)
//            )
//          }
          
          
        } // else isEmpty
      } // FileDropHandler.FilesDropped
    } // reactions
  }
    
  object QueryPanel extends ScrollPane {
    
    verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    
    val ColNum      = 0
    val ColQuery    = 1
    val ColProgress = 2
    
//    case class QueryRow(num: Int, query: MovieQuery) {
//      var progress = new ProgressBar {
//        indeterminate = true
//      }
//    }
//    
//    val tblModel = new javax.swing.table.AbstractTableModel {
//      val colNames = Array("#", "Term", "Progress")
//      val rows = new scala.collection.mutable.ArrayBuffer[QueryRow]()
//      
//      def getColumnCount() = colNames.length
//      override def getColumnName(col: Int) = colNames(col)
//            
//      def getRowCount() = rows.length
//      
//      def getValueAt(rowNum: Int, colNum: Int): AnyRef = {
//        val row = rows(rowNum)
//        colNum match {
//          case ColNum      => row.num.asInstanceOf[AnyRef]
//          case ColQuery    => row.query.term
//          case ColProgress => row.progress 
//        }
//      }
//      
//      def clearRows(): Unit = {
//        rows.clear
//      }
//      
//      def addRow(row: QueryRow): Unit = {
//        rows += row
//        val rowNum = rows.length-1
//        fireTableRowsInserted(rowNum, rowNum)
//      }
//    }
//    
//    val tbl = new Table {
//      preferredViewportSize = new Dimension(500, 70)
//      model = tblModel
//      
//      override protected def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, col: Int): Component = {
//        if (col == ColProgress) { new Component {
//          override lazy val peer = new ProgressBarCellRenderer
//        }} else {
//          super.rendererComponent(isSelected, focused, row, col)              
//        }
//      }
//    }
//    
//    contents = tbl
  }
  
//  object SamePartsPanel extends MigPanel("wrap 2", "[pref!][grow,fill]") {
////    border = EtchedBorder
//        
//    MigPanel.addSeparatorTo(this, "Same Parts")
//    
//    add(new Label("Names"))
//    val txtName = new TextField {
//      columns = 20
//      editable = false
//    }
//    add(txtName)
//        
//    add(new Label("Tags"))
//    val txtTags = new TextField {
//      columns = 20
//      editable = false
//    } 
//    add(txtTags)
//    
//    add(new Label("Year"))
//    val txtYear = new TextField {
//      columns = 4
//      editable = false
//    }
//    add(txtYear)
//  }
  
//  object AllPartsPanel extends MigPanel("wrap 2", "[pref!][grow,fill]") {
////    border = EtchedBorder
//        
//    MigPanel.addSeparatorTo(this, "All Parts")
//    
//    add(new Label("Names"))
//    val txtName = new TextField {
//      columns = 20
//      editable = false
//    }
//    add(txtName)
//        
//    add(new Label("Tags"))
//    val txtTags = new TextField {
//      columns = 20
//      editable = false
//    } 
//    add(txtTags)
//    
//    add(new Label("Year"))
//    val txtYear = new TextField {
//      columns = 4
//      editable = false
//    }
//    add(txtYear)
//  }
}