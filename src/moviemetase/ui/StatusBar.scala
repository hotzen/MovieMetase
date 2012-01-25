package moviemetase
package ui

import scala.swing._
import java.awt.Dimension
import javax.swing.BorderFactory
import java.awt.{Color, Font}

class StatusBar(val top: UI) extends MigPanel("fillx, aligny center") {
  
  add(new Label {
    text = "Tasks: " 
  })
  
  val taskCountLbl = new TextField {
    text    = ""
    columns = 3
    editable = false
    enabled = false
    font = new Font(Font.MONOSPACED, Font.PLAIN, 11)
  }
  add(taskCountLbl, "push")

  val progressBar = new ProgressBar {
    indeterminate = true
    min = 0
    max = 100
    visible = false
  }
  add(progressBar, "dock east, width 50%, height 20!")
  
  listenTo( TaskManager.progress )
  reactions += {
    case TaskManager.ActiveTasks(count) => {
  
      progressBar.visible =
        if (count > 0) true
        else false
     
      taskCountLbl.text =
        if (count > 0) count.toString
        else "-"
    }
  }
}