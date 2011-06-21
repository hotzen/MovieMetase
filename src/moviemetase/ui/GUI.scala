package moviemetase
package ui

import scala.swing._
import scala.swing.event._
import javax.swing.JOptionPane
import javax.imageio.ImageIO
import java.net.URL
import java.awt.image.BufferedImage
import javax.swing.border.EtchedBorder

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
  
  object Main extends MigPanel("", "[fill][grow,fill]", "[grow,fill][][]") {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    
    add(DropTarget, "cell 0 0")
    add(Query, "cell 1 0")
    
    val split = new SplitPane(
      Orientation.Vertical,
      Results,
      ImgPreview
    )
    add(split, "cell 0 1 2 1")
    
    //add(ImgPreview, "cell 2 0 1 2")
    //add(Results, "cell 0 1 2 1")
    add(StatusBar, "cell 0 2 3 1")
  }
  
  
  
  object DropTarget extends Label {
    override lazy val peer: javax.swing.JLabel = new JImageLabel( App.image("/res/drop.png") )
        
    val dropHandler = new FileDropHandler
    listenTo(dropHandler)
    peer.setTransferHandler(dropHandler)
    
//    border  = EtchedBorder
    tooltip = "DROP FILE HERE"
    
    reactions += { case FileDropHandler.FilesDropped(files) => {

      if (files.isEmpty)
        JOptionPane.showMessageDialog(null, "Invalid File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      else if (!files.tail.isEmpty)
        JOptionPane.showMessageDialog(null, "Please drop exactly 1 File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      else if (files.head.isDirectory)
        JOptionPane.showMessageDialog(null, "Please drop exactly one File, no Directory", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      
      else {
        val s: SearchSupervisor[Movie] = new MovieSearch
        val res = s.searchByFile( FileInfo.create( files.head ) )
        publish( Events.Results(res) )
      }
    }}
  }
  
  object Query extends MigPanel() {
    add(new Label("Query"))
  }
  
  object Results extends ScrollPane {
    
    case class Row(score: Double, title: String, year: Int) extends TableModelRow {
      def value(i: Int): AnyRef = { i match {
        case 0 => score
        case 1 => title
        case 2 => year
      }}.asInstanceOf[AnyRef]
    }
    
    val cols =
      TableModel.Col("Score", Some(50)) ::
      TableModel.Col("Title") ::
      TableModel.Col("Year", Some(50)) ::
      Nil
    
    val mdl = TableModel(cols)
    
    val tbl = new Table {
      model    = mdl
      showGrid = true
    }
    contents = tbl
    
    mdl.registerColWidth(tbl)
        
    listenTo( DropTarget )
    reactions += { case Events.Results(res) => {
      for ( (score, movie) <- res) {
        println(score + "/" + movie)
        mdl addRow Row(score, movie.title, movie.year)
        
        val img = movie.infos.collect({ case MovieInfos.Poster(_,optPreviewUrl) => optPreviewUrl }).flatten.head
        ImgPreview.display( new URL(img) )
      }
    }}
  }
  
  
  object ImgPreview extends ScrollPane {
    
    var DefaultSize = new Dimension(300, 600)
    var image: Option[BufferedImage] = None
    
    contents = new Label {
      override def preferredSize(): Dimension = image match {
        case Some(img) => new Dimension(img.getWidth(), img.getHeight())
        case None      => DefaultSize
      }
            
      override def paintComponent(g: Graphics2D): Unit = image match {
        case Some(img) => g.drawImage(img, 0, 0, null)
        case None      => ()
      }
    }
    
    def display(url: URL) {
      WorkerPool submit { try {
        image = Some( ImageIO.read( url ) )
        contents.head.revalidate()
        contents.head.repaint()
      } catch { case e:Exception => {
        image = None
        e.printStackTrace()
      }}}
    }
  }
  
  object StatusBar extends MigPanel() {
    add(new Label("Status"))
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
  
  object Events {
    case class Results(res: List[(Double,Movie)]) extends Event
  }
}