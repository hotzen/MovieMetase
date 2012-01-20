package moviemetase
package ui

import search._
import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
import java.awt.EventQueue

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
      top.visible = true
    }
  }
  
  // run in Event-Dispatch-Thread
  def run(block: => Any): Unit =
    if (EventQueue.isDispatchThread)
      block
    else
      Swing.onEDT { block }

  // publish in EDT
  def publish(pub: Publisher)(evt: Event): Unit =
    run { pub publish evt }
  
  import java.awt.Desktop
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
  val resultPanel = new ResultPanel(this)
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
        topComponent    = resultPanel
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
  
  override val title = App.name + " " + App.version

  override def closeOperation = App.shutdown()
}
