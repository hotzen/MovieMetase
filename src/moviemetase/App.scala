package moviemetase

object App {
  val name    = "MovieMetase"
  val version = "0.1"
  
  def main(args: Array[String]): Unit = {
    
    // please fail fast
    Analyzer.init()
    
    if (args.isEmpty) {
      ui.UI.start()
    } else {
      //TODO CLI?
    }
    
    ()
  }
  
  def shutdown() = {
    println("shutting down...")
    println("please wait until pending tasks have been properly shutdown")
    TaskManager.shutdown()
    System.exit(0)
  }
  
  //lazy val Toolkit = java.awt.Toolkit.getDefaultToolkit
  
  def resource(path: String): java.net.URL = {
    val url = this.getClass.getResource(path)
    if (url == null)
      throw new Exception("resource '" + path + "' does not exist")
    url
  }
    
  
//  def icon(path: String): javax.swing.ImageIcon =
//    new javax.swing.ImageIcon( resource(path) )
  
  def image(path: String): java.awt.Image =
    new javax.swing.ImageIcon( resource(path) ).getImage
    

//  def userDir: java.io.File = 
//	  new java.io.File( util.Properties.userDir )
//    
//  def resourceFromUserDirectory(path: String): java.io.File =
//    new java.io.File(userDir, path)
  
  def configDir: java.io.File = {
    val f = new java.io.File(util.Properties.userDir, "MovieMetase")
    if (!f.exists)
      f.mkdir()
    f
  }
}