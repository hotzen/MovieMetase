package moviemetase
package ui

import scala.swing._

class StatusBar(val top: UI) extends MigPanel {
  add(new Label("Status"))
}