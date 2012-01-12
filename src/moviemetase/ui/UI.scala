package moviemetase
package ui

import search._
import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import javax.imageio.ImageIO
import java.net.URL
import java.awt.image.BufferedImage
import java.awt.EventQueue

object UI {
  
  def start(): Unit = {
    import javax.swing.UIManager
    
    // native Look&Feel
    UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() )
    javax.swing.JFrame.setDefaultLookAndFeelDecorated(true)
    
    // apple integration
    val props = System.getProperties
    props setProperty("apple.laf.useScreenMenuBar", "true")
    props setProperty("com.apple.mrj.application.apple.menu.about.name", App.name + " " + App.version)
    
    Swing.onEDT {
      val top = new UI
      top.pack()
      top.visible = true
    }
  }
  
  // EDT-safe publisher
  def publish(pub: Publisher)(evt: Event): Unit = {
    import java.awt.EventQueue
    if (EventQueue.isDispatchThread)
      pub publish evt
    else
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = {
          pub publish evt
        }
      })
  }
    
  import javax.swing.BorderFactory
  import javax.swing.border._
  
  def border(label: String): Border = {
    val b = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
    BorderFactory.createTitledBorder(b, label);
  }
  
  import java.awt.Desktop
  lazy val desktop: Option[Desktop] =
    try {
      if (Desktop.isDesktopSupported)
        Some( Desktop.getDesktop )
      else
        None
    } catch {
      case e:Exception => None
    }
}

class UI extends Frame {
    
  val dropPanel   = new DropPanel(this)
  val searchPanel = new SearchPanel(this)
  val resultPanel = new ResultPanel(this)
  val infoPanel   = new InfoPanel(this)
  val statusBar   = new StatusBar(this)
  
  contents = new MigPanel("fill") {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    
    val top = new MigPanel("fill", "[][grow,fill]") {
      add(dropPanel, "width 120!, height 120!")
      add(searchPanel, "height 120!")
    }
    add(top, "dock north, grow, height 130!")
    
    add(resultPanel, "wrap, grow, height 100:300:")
    add(infoPanel, "wrap, grow, height 100:200")
    
    add(statusBar, "dock south, height 20!")
  }
  
  override val title = App.name + " " + App.version

  override def closeOperation = App.shutdown()
  
  
  
//  lazy val moviesPanel = new ScrollPane {
//    border = createBorder("Results")
//    
//    case class Row(score: Double, title: String, year: Int, imdb: String, tmdb: String, obj: Movie) extends TableModelRow {
//      def value(i: Int): AnyRef = { i match {
//        case 0 => score
//        case 1 => title
//        case 2 => year
//        case 3 => imdb
//        case 4 => tmdb
//      }}.asInstanceOf[AnyRef]
//    }
//    
//    val cols =
//      TableModel.Col("Score", 50)  ::
//      TableModel.Col("Title", 200) ::
//      TableModel.Col("Year",  50)  ::
//      TableModel.Col("IMDB",  250) ::
//      TableModel.Col("TMDB",  250) ::
//      Nil
//    
//    val mdl = TableModel[Row](cols)
//    
//    val tbl = new Table {
//      model    = mdl
//      showGrid = true
//      selection.intervalMode = Table.IntervalMode.Single
//    }
//    contents = tbl
//    mdl.setPrefSize(tbl, 300)
//        
//    listenTo( dropPanel )
//    listenTo( tbl.selection )
//    
//    reactions += {
//      case Events.SearchResult(movies) => {
//        mdl.clear
//        
//        for ( movie <- movies) {
//          val score = 0 /* {
//            val scores = movie.infos.collect({ case MovieInfos.Score(score) => score })
//              if (scores.isEmpty) 0.0
//              else scores.head
//          } */
//          
//          val imdb = {
//            val imdbs = movie.infos.collect({ case MovieInfos.IMDB(url) => url })
//            if (imdbs.isEmpty) ""
//            else imdbs.head
//          }
//
//          val tmdb = {
//            val tmdbs = movie.infos.collect({ case MovieInfos.TMDB(url) => url })
//            if (tmdbs.isEmpty) ""
//            else tmdbs.head
//          }
//          
//          mdl add Row(score, movie.title, movie.year, imdb, tmdb, movie)
//        }
//      }
//      case TableRowsSelected(src, rng, false) => {
//        for (rowIdx <- src.selection.rows) {
//          val row = mdl.rows(rowIdx)
//          publish( Events.SelectedMovieResult(row.obj) )
//        }
//      }
//    }
//  }
//  
//  lazy val ImagesPanel = new ScrollPane {
//    border = createBorder("Images")
//    
//    case class Row(var checked: Boolean, imgType: String, url: URL, previewUrl: Option[URL], obj: MovieInfos.Image) extends TableModelRow {
//      def value(i: Int): AnyRef = { i match {
//        case 0 => checked
//        case 1 => imgType
//        case 2 => url.toString
//        case 3 => previewUrl.toString
//      }}.asInstanceOf[AnyRef]
//    }
//    
//    val cols =
//      TableModel.CheckboxCol("") ::
//      TableModel.Col("Type", 100) ::
//      TableModel.Col("URL", 680) ::
//      Nil
//    
//    val mdl = TableModel[Row](cols)
//    
//    val tbl = new Table {
//      model    = mdl
//      showGrid = true
//    }
//    contents = tbl
//    mdl.setPrefSize(tbl, 300)
//
//    listenTo( moviesPanel )
//    listenTo( tbl.selection )
//    
//    reactions += {
//      case Events.SelectedMovieResult(movie) => {
//        mdl.clear
//        for (img <- movie.infos.collect({ case img: MovieInfos.Image => img})) {
//          val imgType = img.getClass.getSimpleName
//          mdl add Row(false, imgType, img.url, img.preview, img)
//        }
//      }
//      case TableRowsSelected(src, rng, false) => {
//        for (rowIdx <- src.selection.rows) {
//          val row = mdl.rows(rowIdx)
//          publish( Events.SelectedImage(row.obj) )
//        }
//      }
//    }
//  }
//  
//  lazy val SubtitlesPanel = new ScrollPane {
//    border = createBorder("Subtitles")
//    
//    case class Row(var checked: Boolean, lang: String, page: String, file: String, obj: MovieInfos.Subtitle) extends TableModelRow {
//      def value(i: Int): AnyRef = { i match {
//        case 0 => checked
//        case 1 => lang
//        case 2 => page
//        case 3 => file
//      }}.asInstanceOf[AnyRef]
//    }
//    
//    val cols =
//      TableModel.CheckboxCol("")      ::
//      TableModel.Col("Language", 100) ::
//      TableModel.Col("Page", 430)     ::
//      TableModel.Col("File", 230)     ::
//      Nil
//    
//    val mdl = TableModel[Row](cols)
//    
//    val tbl = new Table {
//      model    = mdl
//      showGrid = true
//    }
//    mdl.setPrefSize(tbl, DefaultTableHeight)
//    contents = tbl
//    
//    listenTo( moviesPanel )
//    //listenTo( tbl.selection )
//    
//    reactions += {
//      case Events.SelectedMovieResult(movie) => {
//        mdl.clear
//        for (sub <- movie.infos.collect({ case sub:MovieInfos.Subtitle => sub})) {
//          mdl add Row(false, sub.lang, sub.page.toString, sub.file.toString, sub)
//        }
//      }
//      case TableRowsSelected(src, rng, false) => {
//        
//      }
//    }
//  }
//  
//  lazy val MovieInfosPanel = new ScrollPane {
//    border = createBorder("All Movie-Infos")
//    
//    case class Row(var checked: Boolean, infoType: String, info: String, source: String, obj: MovieInfo) extends TableModelRow {
//      def value(i: Int): AnyRef = { i match {
//        case 0 => checked
//        case 1 => infoType
//        case 2 => info
//        case 3 => source
//      }}.asInstanceOf[AnyRef]
//    }
//    
//    val cols =
//      TableModel.CheckboxCol("")    ::
//      TableModel.Col("Type", 100)   ::
//      TableModel.Col("Info", 530)   ::
//      TableModel.Col("Source", 100) ::
//      Nil
//    
//    val mdl = TableModel[Row](cols)
//    
//    val tbl = new Table {
//      model    = mdl
//      showGrid = true
//    }
//    mdl.setPrefSize(tbl, DefaultTableHeight)
//    contents = tbl
//    
//    listenTo( moviesPanel )
//    //listenTo( tbl.selection )
//    
//    def infoSorter(a: MovieInfo, b: MovieInfo): Boolean = {
//      val aCl = a.getClass.getSimpleName
//      val bCl = b.getClass.getSimpleName
//      aCl <= bCl
//    }
//    
//    reactions += {
//      case Events.SelectedMovieResult(movie) => {
//        mdl.clear
//        for (info <- movie.infos.sortWith(infoSorter)) {
//          val infoType = info.getClass.getSimpleName
//          mdl add Row(false, infoType, info.toString, info.source, info)
//        }
//      }
//    }
//  }
  
//  lazy val ImgPreviewPanel = new ScrollPane {
//    var DefaultSize = new Dimension(300, DefaultTableHeight)
//    var image: Option[BufferedImage] = None
//       
////    def createScaler(img: BufferedImage, targetWidth: Int, targetHeight: Int): BufferedImageOp = new RescaleOp {
////      null
////    }
//    
//    val imgLbl = new Label {
////      override def preferredSize(): Dimension = image match {
////        case Some(img) => new Dimension(img.getWidth(), img.getHeight())
////        case None      => DefaultSize
////      }
//      override def paintComponent(g: Graphics2D): Unit = image match {
//        case Some(img) => {
//          val scaler = null // createScaler(img, 100, 200) 
//          g.drawImage(img, scaler, 0, 0)
//        }
//        case None => {
//          ()
//        }
//      }
//    }
//    contents = imgLbl
//    
//    def display(url: URL): Unit = {
////      Task.create[Unit]({
////        try {
////          image = Some( ImageIO.read( url ) )
////          contents.head.revalidate()
////          contents.head.repaint()
////        } catch { case e:Exception => {
////          image = None
////          e.printStackTrace()
////        }}
////      }).submit()
////      
////      ()
//    }
//    
//    listenTo( ImagesPanel )
//    reactions += {
//      case Events.SelectedImage(img) => {
//        if (img.preview.isEmpty)
//          display( img.url )
//        else
//          display( img.preview.head )
//      }
//    }
//  }
}

object Events {

//  case class SearchResult(movies: List[Movie]) extends Event
//  case class SelectedMovieResult(movie: Movie) extends Event
//  case class SelectedImage(img: MovieInfos.Image) extends Event
    
  // publish in Event-Dispatch-Thread
  

}