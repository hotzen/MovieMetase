package moviemetase.ui.comp

import scala.swing._

class SeqPanel extends Panel with SequentialContainer.Wrapper {
  def clear(): Unit = contents.clear()
  
  def add(comp: JImage) {
    contents += Component.wrap( comp )
  }
}