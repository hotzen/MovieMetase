package moviemetase
package ui

import scala.swing._
import java.awt.Dimension
import javax.swing.BorderFactory
import java.awt.Color

class StatusBar(val top: UI) extends MigPanel("fillx, aligny center") {
  
  add(new Label {
    text = "Tasks: " 
  })
  
  val taskCountLbl = new TextField {
    text    = ""
    columns = 3
    editable = false
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
        else ""

//      progressBar.visible = true
//      if (count > 0) {
//        taskCountLbl.text = count.toString
//        
//        progressBar.indeterminate = true
//      } else {
//        taskCountLbl.text = "OK"
//          
//        progressBar.indeterminate = false
//        progressBar.value = 100
//      }
    }
  }
}