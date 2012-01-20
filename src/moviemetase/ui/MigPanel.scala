package moviemetase
package ui

import scala.swing.{Panel, Component, LayoutContainer}
import net.miginfocom.swing.MigLayout
import javax.swing.JComponent

object MigPanel {
//  var SeparatorColor = new scala.swing.Color(0, 70, 213)
    
//  def addSeparatorTo(panel: MigPanel, label: String): MigPanel = {
//    import scala.swing.{Label, Separator}
//    
//    val lbl = new Label(label)
//    lbl.foreground = SeparatorColor
//    
//    val sep = new Separator
//    
//    panel.add(lbl, "gapbottom 1, span, split 2, aligny center")
//    panel.add(sep, "gapleft rel, growx")
//    panel
//  }  
}

class MigPanel(
  val layoutConstraints: String = "",
  val columnConstraints: String = "",
  val rowConstraints: String    = "") extends Panel with LayoutContainer {
  
  lazy val mig = new MigLayout(
    layoutConstraints,
    columnConstraints,
    rowConstraints
  ) 
  
  override lazy val peer =
    new javax.swing.JPanel(mig) with SuperMixin
  
  type Constraints = String
  
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  
  protected def constraintsFor(comp: Component): Constraints =
    layoutManager.getComponentConstraints(comp.peer).asInstanceOf[String]
  
  protected def areValid(constr: Constraints): (Boolean, String) = (true, "")
  
  def add(comp: JComponent, constr: String): Unit = peer.add(comp, constr)
  
  def add(comp: Component, constr: String = ""): Unit  = peer.add(comp.peer, constr)
  
  def clear(): Unit = {
    for (comp <- peer.getComponents) {
      peer.remove( comp )
    }
  }
}