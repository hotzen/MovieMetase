package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.{Desktop, Color, Font}
import javax.swing.{BorderFactory, JButton}
import javax.swing.border.{BevelBorder}
import java.net.URL
import javax.imageio.ImageIO
import java.awt.image.{RenderedImage, BufferedImage}
import javax.swing.JLabel

object ResultPanel {
  
}



class ResultPanel(val top: UI) extends ScrollPane {
  
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  
  val panel = new MigPanel("wrap, fillx", "", "")
  contents = panel

  def render(cont: MigPanel, movie: Movie): Unit = {
    cont.border = BorderFactory.createLineBorder(Color.GRAY)
    
    // title / year
    cont.add(new Label {
      text = {
        if (movie.year > 0)
          movie.title + " (" + movie.year + ")"
        else
          movie.title
      }
      font = new Font(Font.SERIF, Font.BOLD, 16)
      border = Swing.EmptyBorder

      xAlignment = Alignment.Left
      yAlignment = Alignment.Top
    }, "dock north")
    
    val images = movie.infos.collect({ case i:MovieInfos.Image => i })
        
    // main-image
    cont.add(new Label {
      text = "Main-Image"
//      images.headOption match {
//        case Some(img) => 
//        case None => 
//      }
    }, "dock east")
    
    cont.add(new MigPanel("fillx") {
      
      // genres
      val genres = movie.infos.collect({ case MovieInfos.Genre(g) => g })
      add(new Label {
        text = genres.map(_.capitalize).sortWith(_ < _).mkString(", ")
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
      }, "push")
      
      // webpages
      for (url <- movie.infos.collect({ case i:MovieInfos.WebPage => i.page })) {
        val host = url.getHost 
        val label = 
          if (host startsWith "www.")
            host.substring(4)
          else
            host
        
        val action = new Action(label) {
          def apply() =
            for (desktop <- UI.desktop)
              desktop.browse( url.toURI )
        }
        
        add(new Button(action) {
          contentAreaFilled = true
          borderPainted     = true
          focusPainted      = false
          opaque            = false
          font = new Font(Font.MONOSPACED, Font.PLAIN, 9)
        })
      }
    }, "growx, shrinkx, wrap")
    
    
    val QuoteBegin = "\u00AB "
    val QuoteEnd   = " \u00BB"
    
    // descriptions
    val descriptions = movie.infos.collect({ case i:MovieInfos.Description => i })
    if (!descriptions.isEmpty) {
      cont.add(new Label("Description"), "wrap")
      
      for (desc <- descriptions.map(_.text)) {
        cont.add(new TextArea {
          text = QuoteBegin + desc + QuoteEnd
          font = new Font(Font.SERIF, Font.PLAIN, 12)
          border = null
          tabSize = 2
          editable = false
          lineWrap = true
          wordWrap = true
          opaque = false
        }, "growx, wrap")
      }
    }
        
    // summaries
    val summaries = movie.infos.collect({ case i:MovieInfos.Summary => i })
    if (!summaries.isEmpty) {
      cont.add(new Label("Summary"), "wrap")
      
      for (summary <- summaries.map(_.text)) {
        cont.add(new TextArea {
          text = QuoteBegin + summary + QuoteEnd
          font = new Font(Font.SERIF, Font.PLAIN, 12)
          border = null
          tabSize = 2
          editable = false
          lineWrap = true
          wordWrap = true
          opaque = false
        }, "growx, wrap")
      }
    }
        
    // images
    if (!images.isEmpty)
      cont.add(new MigPanel("fillx") {
        
        for (url <- images.map(_.url)) {
          val imgLbl = new ImageLabel(url, 400)
          add(imgLbl, "")
        }
      
      }, "growx")
  }

  listenTo( top.searchPanel )
  reactions += {
    case SearchRowSelected(row) => {
      panel.clear()
      
      for (movie <- row.movies) {
        val cont = new MigPanel("fillx")
        render(cont, movie)
        panel.add(cont, "grow")
      }
      
      panel.revalidate()
      panel.repaint()
    }
  }

  { // TEST
    val infos =
      MovieInfos.Title("Inception") ::
      MovieInfos.Release(2010) :: 
      MovieInfos.Genre("Drama") ::
      MovieInfos.Genre("SciFi") ::
      MovieInfos.Genre("Doener") ::
      MovieInfos.Genre("Thriller") ::
      MovieInfos.IMDB("tt123456") :: 
      MovieInfos.TMDB("123456") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(top.searchPanel)( SearchRowSelected(row) )
  }
}

object ImageLabel {
  lazy val PlaceholderImage: BufferedImage = ImageIO.read( App.resource("/res/image-loading.png") ) 
}

// http://today.java.net/pub/a/today/2007/04/03/perils-of-image-getscaledinstance.html
class ImageLabel(url: URL, width: Int) extends JLabel {
  import java.awt.Graphics
  import java.awt.RenderingHints
  
  setOpaque(true)
    
  private var img: BufferedImage = ImageLabel.PlaceholderImage
    
  private val loadingTask = new Task[Unit] {
    def execute(): Unit =
      try {
        val img = ImageIO.read(url)
        if (img != null)
          UI.run {
            val that = ImageLabel.this
            that.img = img
            that.revalidate()
            that.repaint()
          }
        else
          println("ImageLabel.loadingTask NULL loaded " + url)
      } catch { case e:Exception => e.printStackTrace() }
  }
  private var loadingTaskSubmitted = false
  
  def load(): Unit = {
    loadingTask.submit()
    loadingTaskSubmitted = true
  }
  
  override def paintComponent(g: java.awt.Graphics): Unit = {
    super.paintComponent(g)
    
    if (!loadingTaskSubmitted)
      load()
    
    val g2 = g.asInstanceOf[Graphics2D]
    val compSize = getSize()
    
    val newW = compSize.width
    val newH = scaleHeight(newW)
        
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    
    g2.drawImage(img, 0, 0, newW, newH, null);
  }
  
  def ratio: Float = img.getWidth.toFloat / img.getHeight
  
  def scaleHeight(w: Int): Int = (w / ratio).toInt
  
  override def getPreferredSize(): Dimension =
    new Dimension(width, scaleHeight(width))
}