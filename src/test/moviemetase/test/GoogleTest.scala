package moviemetase
package test

import sites._
import search._
import scala.util.Random
import java.io.PrintWriter
import moviemetase.ui.comp.JImage

object GoogleTest extends swing.Reactor {
  def main(args: Array[String]) = {
    Logging.out = new PrintWriter(System.out)
    Logging.level = LogLevel.Trace
    
    
    val rnd = new Random
    for (i <- 1 to 1000) {
      val cs = for (i <- 1 to 10) yield rnd.nextPrintableChar
      val res = Google.Query("foobar " + cs.mkString("")).execute()
      println(i + " = " + res.length + "\n-----")
    }

    ()
  }
  
  listenTo( HumanTasks )
  reactions += {
    case e@sites.GoogleWeb.CaptchaRequired(challenge, _) => {
      import java.awt._
      import javax.swing._
      println("EVENT: " + e)
      
      val panel = new JPanel(new BorderLayout)
              
      val lbl = new JLabel()
      lbl.setText("Please enter the CAPTCHA:")
      panel.add(lbl, BorderLayout.NORTH)
      
      val img = new JImage(challenge.img, None, JImage.Blocking, JImage.NoCaching)
      panel.add(img, BorderLayout.SOUTH)
      
      var prompt: String = null
      while (prompt == null) {
        prompt = JOptionPane.showInputDialog(panel)
      }
      
      val resp = sites.GoogleWeb.CaptchaResponse(prompt, challenge)
      println("replying: " + resp)
      e reply resp        
    }
  }
}