package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import java.io.Writer
import java.awt.Font

class JTextWriter(area: javax.swing.JTextArea, autoscroll: Boolean) extends java.io.Writer {
  import javax.swing.text.DefaultCaret
  
  if (autoscroll) {
    area.getCaret.asInstanceOf[DefaultCaret] setUpdatePolicy DefaultCaret.ALWAYS_UPDATE
  }
  
  override def flush(): Unit = ()
  override def close(): Unit = ()
  
  override def write(cs: Array[Char], off: Int, len: Int): Unit =
    append( new String(cs, off, len) )
  
  def append(s: String): Unit = java.awt.EventQueue.invokeLater(new Runnable {
    def run(): Unit = area append s
  })
}

class LogPanel(val top: UI) extends ScrollPane {
  
  val textArea = new TextArea(3, 80)
  textArea.editable = false
  textArea.tabSize = 2
  textArea.font = new Font(Font.MONOSPACED, Font.PLAIN, 9)
  
  val writer = new JTextWriter( textArea.peer, true )
  
  contents = textArea
  
  Logging.out = new java.io.PrintWriter( writer )
}