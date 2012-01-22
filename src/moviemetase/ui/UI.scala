package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event.Event
import java.awt.{Toolkit, Desktop, EventQueue}


object UI {
  
  def start(): Unit = {
    import javax.swing.UIManager
    
    // native Look&Feel
    UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() )
    javax.swing.JFrame.setDefaultLookAndFeelDecorated(true)
    
    // apple integration
    val props = System.getProperties
    props.setProperty("apple.laf.useScreenMenuBar", "true")
    props.setProperty("com.apple.mrj.application.apple.menu.about.name", App.name + " " + App.version)
    
    Swing.onEDT {
      val top = new UI
      top.pack()
      
      val screenSize = toolkit.getScreenSize
      top.size = new Dimension(
        (screenSize.width * 0.8).toInt,
        (screenSize.height * 0.8).toInt
      )
      top.visible = true
    }
  }
  
  
  // run in Event-Dispatch-Thread
  def run(block: =>Any): Unit =
    if (EventQueue.isDispatchThread)
      block
    else
      Swing.onEDT { block }

  // publish in EDT
  def publish(pub: Publisher)(evt: Event): Unit =
    run { pub publish evt }
  
  
  val toolkit = Toolkit.getDefaultToolkit
  
  val desktop: Option[Desktop] =
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
  val moviePanel  = new MoviePanel(this)
  val infoPanel   = new InfoPanel(this)
  val logPanel    = new LogPanel(this)
  val statusBar   = new StatusBar(this)
  
  contents = new MigPanel("fill") {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    
    val top = new MigPanel("fill", "[][grow,fill]") {
      add(dropPanel, "width 120!, height 120!")
      add(searchPanel, "height 120!")
    }
    add(top, "dock north, grow, height 130!")

        
    add(new SplitPane {
      topComponent  = new SplitPane {
        topComponent    = moviePanel
        bottomComponent = infoPanel
    
        resizeWeight = 0.5 // auto-resize even
        oneTouchExpandable = false 
      }
      
      bottomComponent = logPanel
      
      resizeWeight = 0.97
      oneTouchExpandable = false
    }, "grow")

    add(statusBar, "dock south, grow, height 25!")
  }
  
  title = App.name + " " + App.version
  iconImage = UI.toolkit.getImage("/res/icon.png")
  
  override def closeOperation = App.shutdown()
}

class ImageLoader(url: java.net.URL, callback: Image => Unit) extends Task[Unit] with Logging {
  import javax.imageio.ImageIO
  
  val logID = "ImageLoader(" + url.toExternalForm + ")"
  
  def execute(): Unit =
    try {
      trace("ImageIO.read() ...")
      val img = ImageIO.read(url)
      if (img != null)
        UI.run { callback(img) }
      else
        error("ImageIO returned NULL")
    } catch {
      case e:Exception => error(e.getMessage(), ("exception" -> e) :: Nil)
    }
}