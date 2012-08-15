package moviemetase
package ui

import scala.swing._
import java.awt.Dimension
import javax.swing.BorderFactory
import java.awt.{Color, Font}
import comp._

class StatusBar(val top: UI) extends MigPanel("fillx, aligny center") {
  
  val progressBar = new ProgressBar {
    min = 0
    max = 100
    visible = false
  }
  add(progressBar, "grow, height 20!")
  
  TaskManager registerOnChange { stats => UI.run {
    progressBar.visible =
      if (stats.active > 0) true
      else false
    
    progressBar.value = stats.completedPercent
    progressBar.paintBorder(true)
    
    progressBar.tooltip = "Active Tasks " + stats.active + " / Completed Tasks " + stats.completed + " / Total Tasks " + stats.total
    //progressBar.label = stats.active + " / " + stats.total
    //progressBar.labelPainted = false // does not look good, weird font
  }}
}