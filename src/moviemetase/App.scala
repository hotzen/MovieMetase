package moviemetase

object App {
    
  def main(args: Array[String]) = {
    (new ui.GUI).start
  }
  
  def shutdown() = {
    println("SHUTDOWN...")
    System.exit(0)
  }
  
  lazy val Toolkit = java.awt.Toolkit.getDefaultToolkit
 
  def image(path: String): java.awt.Image =
    (new javax.swing.ImageIcon( resource(path) )).getImage
    
  def resource(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
  
  def classExists(qname: String): Boolean =
    try {
      Class.forName(qname, false, null)
      true
    } catch {
      case e:ClassNotFoundException => false
    }
}