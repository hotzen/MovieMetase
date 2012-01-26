package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import java.io.Writer
import java.awt.Font
import scala.swing.event.ButtonClicked

class JTextWriter(area: javax.swing.JTextArea, autoscroll: Boolean) extends java.io.Writer {
  import javax.swing.text.DefaultCaret
  
  if (autoscroll) {
    area.getCaret.asInstanceOf[DefaultCaret] setUpdatePolicy DefaultCaret.ALWAYS_UPDATE
  }
  
  override def flush(): Unit = ()
  override def close(): Unit = ()
  
  override def write(cs: Array[Char], off: Int, len: Int): Unit =
    append( new String(cs, off, len) )
  
  def append(s: String): Unit =
    UI run { area append s }
}

class LogPanel(val top: UI) extends MigPanel("fill") {
  
  minimumSize = new Dimension(0, 0)
  
  val logFont = new Font(Font.MONOSPACED, Font.PLAIN, 10)
  val btnFont = logFont
  
  val Trace = new RadioButton("tracing")  { font = btnFont }
  val Info  = new RadioButton("infos")    { font = btnFont }
  val Warn  = new RadioButton("warnings") { font = btnFont }
  val Error = new RadioButton("errors")   { font = btnFont }
    
  val group = new ButtonGroup(Trace, Info, Warn, Error)
  
  val buttons = new MigPanel {
    add(Trace, Info, Warn, Error)
  }
  add(buttons, "dock north, gap 0")
    
  val textArea = new TextArea(3, 80)
  textArea.editable = false
  textArea.tabSize = 2
  textArea.font = logFont
  
  val writer = new JTextWriter(textArea.peer, true)
  Logging.out = new java.io.PrintWriter( writer )
  
  add(new ScrollPane(textArea), "grow")
    
  listenTo(Trace, Info, Warn, Error)
  reactions += {
    case ButtonClicked(Trace) => Logging.level = LogLevel.Trace
    case ButtonClicked(Info)  => Logging.level = LogLevel.Info
    case ButtonClicked(Warn)  => Logging.level = LogLevel.Warn
    case ButtonClicked(Error) => Logging.level = LogLevel.Error
  }
  
  Trace.doClick()
}