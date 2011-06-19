package moviemetase
package ui

import scala.swing.{Panel, Component, LayoutContainer}
import net.miginfocom.swing.MigLayout

object MigPanel {
  var SeparatorColor = new scala.swing.Color(0, 70, 213)
  
  def addSeparatorTo(panel: MigPanel, label: String): MigPanel = {
    import scala.swing.{Label, Separator}
    
    val lbl = new Label(label)
    lbl.foreground = SeparatorColor    
    
    val sep = new Separator
    
    panel.add(lbl, "gapbottom 1, span, split 2, aligny center")
    panel.add(sep, "gapleft rel, growx")
    panel
  }  
}

class MigPanel(
  val layoutConstraints: String = "",
  val columnConstraints: String = "",
  val rowConstraints: String    = "") extends Panel with LayoutContainer {
  
  override lazy val peer = {
    val mig = new MigLayout(
      layoutConstraints,
      columnConstraints,
      rowConstraints
    )
    new javax.swing.JPanel(mig) with SuperMixin
  }
  
  type Constraints = String
  
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  
  protected def constraintsFor(comp: Component): Constraints =
    layoutManager.getComponentConstraints(comp.peer).asInstanceOf[String]
  
  protected def areValid(constr: Constraints): (Boolean, String) = (true, "")
  
  def add(comp: Component, constr: String): Unit = peer.add(comp.peer, constr)

  def add(comp: Component): Unit = add(comp, "")
}