package moviemetase
package ui

//import search.{SearchManager, MovieSearch}
import search._

import scala.swing._
import scala.swing.event._
import javax.swing.JOptionPane
import javax.imageio.ImageIO
import java.net.URL
import java.awt.image.BufferedImage
import javax.swing.border.Border
import javax.swing.border.EtchedBorder
import javax.swing.BorderFactory
import java.awt.image.BufferedImageOp
import java.awt.image.RescaleOp

class GUI extends Reactor {
  val Title = "MovieMetase"
  
  val DefaultTableHeight = 300
    
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
  
  def createBorder(label: String): Border = {
    val b = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
    BorderFactory.createTitledBorder(b, label);
  }
  
  lazy val Top = new Frame {
    title = Title
    
    contents = new MigPanel("fill") {
      border = Swing.EmptyBorder(5, 5, 5, 5)
          
      val top = new MigPanel() {
        add(DropPanel)
        add(SearchPanel, "grow")
      }
      add(top, "dock north")
      
      val imgSplit = new SplitPane(Orientation.Vertical, ImagesPanel, ImgPreviewPanel) {
        resizeWeight = 0.5
      }
      
      val tabbed = new TabbedPane {
        pages += new TabbedPane.Page("1.) Select one Movie", MoviesPanel)
        pages += new TabbedPane.Page("2.) Select Images", imgSplit)
        pages += new TabbedPane.Page("3.) Select Subtitles", SubtitlesPanel)
        pages += new TabbedPane.Page("4.) Select additional Information", MovieInfosPanel)
      }
      add(tabbed, "grow")
          
      add(StatusPanel, "dock south")
    }
    
    override def closeOperation = App.shutdown()
  }
  
  lazy val DropPanel = new Label {
    override lazy val peer: javax.swing.JLabel = new JImageLabel( App.image("/res/drop.png") )
    
    tooltip = "DROP FILE HERE"
      
    val dropHandler = new FileDropHandler
    listenTo(dropHandler)
    peer.setTransferHandler(dropHandler)
    
    reactions += { case FileDropHandler.FilesDropped(files) => {
  
      if (files.isEmpty)
        JOptionPane.showMessageDialog(null, "Invalid File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      else if (!files.tail.isEmpty)
        JOptionPane.showMessageDialog(null, "Please drop exactly 1 File", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      else if (files.head.isDirectory)
        JOptionPane.showMessageDialog(null, "Please drop exactly one File, no Directory", "Dropped Files", JOptionPane.ERROR_MESSAGE);
      
      else {
        val s: SearchManager[Movie] = new MovieSearch
        val res = s.searchByFile( FileInfo( files.head ) )
        publish( Events.MovieResults(res) )
      }
    }}
  } 

  
  lazy val SearchPanel = new MigPanel() {
    add(new Label("Queryyyyyyy"))
  }
  
  lazy val MoviesPanel = new ScrollPane {
    border = createBorder("Results")
    
    case class Row(score: Double, title: String, year: Int, imdb: String, tmdb: String, obj: Movie) extends TableModelRow {
      def value(i: Int): AnyRef = { i match {
        case 0 => score
        case 1 => title
        case 2 => year
        case 3 => imdb
        case 4 => tmdb
      }}.asInstanceOf[AnyRef]
    }
    
    val cols =
      TableModel.Col("Score", 50)  ::
      TableModel.Col("Title", 200) ::
      TableModel.Col("Year",  50)  ::
      TableModel.Col("IMDB",  250) ::
      TableModel.Col("TMDB",  250) ::
      Nil
    
    val mdl = TableModel[Row](cols)
    
    val tbl = new Table {
      model    = mdl
      showGrid = true
      selection.intervalMode = Table.IntervalMode.Single
    }
    contents = tbl
    mdl.setPrefSize(tbl, DefaultTableHeight)
        
    listenTo( DropPanel )
    listenTo( tbl.selection )
    
    reactions += {
      case Events.MovieResults(res) => {
        mdl.clear
        
        for ( movie <- res) {
          val score = 0 /* {
            val scores = movie.infos.collect({ case MovieInfos.Score(score) => score })
              if (scores.isEmpty) 0.0
              else scores.head
          } */
          
          val imdb = {
            val imdbs = movie.infos.collect({ case MovieInfos.IMDB(url) => url })
            if (imdbs.isEmpty) ""
            else imdbs.head
          }

          val tmdb = {
            val tmdbs = movie.infos.collect({ case MovieInfos.TMDB(url) => url })
            if (tmdbs.isEmpty) ""
            else tmdbs.head
          }
          
          mdl add Row(score, movie.title, movie.year, imdb, tmdb, movie)
        }
      }
      case TableRowsSelected(src, rng, false) => {
        for (rowIdx <- src.selection.rows) {
          val row = mdl.rows(rowIdx)
          publish( Events.SelectedMovieResult(row.obj) )
        }
      }
    }
  }
  
  lazy val ImagesPanel = new ScrollPane {
    border = createBorder("Images")
    
    case class Row(var checked: Boolean, imgType: String, url: URL, previewUrl: Option[URL], obj: MovieInfos.Image) extends TableModelRow {
      def value(i: Int): AnyRef = { i match {
        case 0 => checked
        case 1 => imgType
        case 2 => url.toString
        case 3 => previewUrl.toString
      }}.asInstanceOf[AnyRef]
    }
    
    val cols =
      TableModel.CheckboxCol("") ::
      TableModel.Col("Type", 100) ::
      TableModel.Col("URL", 680) ::
      Nil
    
    val mdl = TableModel[Row](cols)
    
    val tbl = new Table {
      model    = mdl
      showGrid = true
    }
    contents = tbl
    mdl.setPrefSize(tbl, DefaultTableHeight)

    listenTo( MoviesPanel )
    listenTo( tbl.selection )
    
    reactions += {
      case Events.SelectedMovieResult(movie) => {
        mdl.clear
        for (img <- movie.infos.collect({ case img: MovieInfos.Image => img})) {
          val imgType = img.getClass.getSimpleName
          mdl add Row(false, imgType, img.url, img.preview, img)
        }
      }
      case TableRowsSelected(src, rng, false) => {
        for (rowIdx <- src.selection.rows) {
          val row = mdl.rows(rowIdx)
          publish( Events.SelectedImage(row.obj) )
        }
      }
    }
  }
  
  lazy val SubtitlesPanel = new ScrollPane {
    border = createBorder("Subtitles")
    
    case class Row(var checked: Boolean, lang: String, page: String, file: String, obj: MovieInfos.Subtitle) extends TableModelRow {
      def value(i: Int): AnyRef = { i match {
        case 0 => checked
        case 1 => lang
        case 2 => page
        case 3 => file
      }}.asInstanceOf[AnyRef]
    }
    
    val cols =
      TableModel.CheckboxCol("")      ::
      TableModel.Col("Language", 100) ::
      TableModel.Col("Page", 430)     ::
      TableModel.Col("File", 230)     ::
      Nil
    
    val mdl = TableModel[Row](cols)
    
    val tbl = new Table {
      model    = mdl
      showGrid = true
    }
    mdl.setPrefSize(tbl, DefaultTableHeight)
    contents = tbl
    
    listenTo( MoviesPanel )
    //listenTo( tbl.selection )
    
    reactions += {
      case Events.SelectedMovieResult(movie) => {
        mdl.clear
        for (sub <- movie.infos.collect({ case sub:MovieInfos.Subtitle => sub})) {
          mdl add Row(false, sub.lang, sub.page.toString, sub.file.toString, sub)
        }
      }
      case TableRowsSelected(src, rng, false) => {
        
      }
    }
  }
  
  lazy val MovieInfosPanel = new ScrollPane {
    border = createBorder("All Movie-Infos")
    
    case class Row(var checked: Boolean, infoType: String, info: String, source: String, obj: MovieInfo) extends TableModelRow {
      def value(i: Int): AnyRef = { i match {
        case 0 => checked
        case 1 => infoType
        case 2 => info
        case 3 => source
      }}.asInstanceOf[AnyRef]
    }
    
    val cols =
      TableModel.CheckboxCol("")    ::
      TableModel.Col("Type", 100)   ::
      TableModel.Col("Info", 530)   ::
      TableModel.Col("Source", 100) ::
      Nil
    
    val mdl = TableModel[Row](cols)
    
    val tbl = new Table {
      model    = mdl
      showGrid = true
    }
    mdl.setPrefSize(tbl, DefaultTableHeight)
    contents = tbl
    
    listenTo( MoviesPanel )
    //listenTo( tbl.selection )
    
    def infoSorter(a: MovieInfo, b: MovieInfo): Boolean = {
      val aCl = a.getClass.getSimpleName
      val bCl = b.getClass.getSimpleName
      aCl <= bCl
    }
    
    reactions += {
      case Events.SelectedMovieResult(movie) => {
        mdl.clear
        for (info <- movie.infos.sortWith(infoSorter)) {
          val infoType = info.getClass.getSimpleName
          mdl add Row(false, infoType, info.toString, info.source, info)
        }
      }
    }
  }
  
  lazy val ImgPreviewPanel = new ScrollPane {
    var DefaultSize = new Dimension(300, DefaultTableHeight)
    var image: Option[BufferedImage] = None
       
//    def createScaler(img: BufferedImage, targetWidth: Int, targetHeight: Int): BufferedImageOp = new RescaleOp {
//      null
//    }
    
    val imgLbl = new Label {
//      override def preferredSize(): Dimension = image match {
//        case Some(img) => new Dimension(img.getWidth(), img.getHeight())
//        case None      => DefaultSize
//      }
      override def paintComponent(g: Graphics2D): Unit = image match {
        case Some(img) => {
          val scaler = null // createScaler(img, 100, 200) 
          g.drawImage(img, scaler, 0, 0)
        }
        case None => {
          ()
        }
      }
    }
    contents = imgLbl
    
    def display(url: URL) {
      TaskExecutor submit { try {
        image = Some( ImageIO.read( url ) )
        contents.head.revalidate()
        contents.head.repaint()
      } catch { case e:Exception => {
        image = None
        e.printStackTrace()
      }}}
    }
    
    listenTo( ImagesPanel )
    reactions += {
      case Events.SelectedImage(img) => {
        if (img.preview.isEmpty)
          display( img.url )
        else
          display( img.preview.head )
      }
    }
  }
  
  lazy val StatusPanel = new MigPanel() {
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
}

object Events {
  case class MovieResults(res: List[Movie]) extends Event
  case class SelectedMovieResult(movie: Movie) extends Event
  case class SelectedImage(img: MovieInfos.Image) extends Event
}