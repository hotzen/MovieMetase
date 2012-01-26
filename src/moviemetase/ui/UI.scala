package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event.Event
import java.awt.{Toolkit, Desktop, EventQueue}
import java.net.URL
import java.awt.Color
import javax.swing.BorderFactory
import javax.swing.border.EtchedBorder

object UI {
  def start(): Unit = {
    import javax.swing.UIManager
    
    try UIManager setLookAndFeel "javax.swing.plaf.nimbus.NimbusLookAndFeel" 
    catch { case e:Exception => UIManager setLookAndFeel UIManager.getSystemLookAndFeelClassName }
        
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
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = { block }
      })

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
  
  //val SelectionColor = new Color(131, 196, 45)
  val SelectionColor = new Color(139, 209, 46)
  
  
  //val SelectionBorder = BorderFactory createLineBorder SelectionColor
  //val SelectionBorder = new EtchedBorder(EtchedBorder.RAISED, SelectionColor, Color.BLACK)
  val SelectionBorder = BorderFactory.createMatteBorder(3, 3, 3, 3, SelectionColor)
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
  
  
  { // TEST
    val infos =
      MovieInfos.Title("Inception") ::
      MovieInfos.Release(2010) :: 
      MovieInfos.Genre("Drama") ::
      MovieInfos.Genre("SciFi") ::
      MovieInfos.Genre("Doener") ::
      MovieInfos.Genre("Thriller") ::
      MovieInfos.Actor("Leonardo DiCaprio", Some("Cobb")) ::
      MovieInfos.Actor("Joseph Gordon-Levitt", Some("Arthur")) ::
      MovieInfos.Director("Christopher Nolan") ::
      MovieInfos.IMDB("tt1375666") :: 
      MovieInfos.TMDB("27205") ::
      MovieInfos.Description("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      MovieInfos.Poster(new URL("http://ia.media-imdb.com/images/M/MV5BMjAxMzY3NjcxNF5BMl5BanBnXkFtZTcwNTI5OTM0Mw@@._V1._SX640_SY948_.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/bNGehW2wIxagZIlZqszECzXZDck.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/9YL3Frgm8LUYnoWPQXskLYYg5XZ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/2VnwCwBvwYgojCvgEB4wPKsMCvF.jpg") ) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/vmcpt1DALqJSLHTOuN6nJSDzupS.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/hloLHcDVdFoB7eFvLiKpQPqieFP.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/zHl0p6NGVdJgFAEjtEZKhmq7EvM.jpg")) ::
      Nil
      
    val movie = Movie(infos).get
    
    val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    UI.publish(searchPanel)( SearchRowSelected(row) )
  }
}