package moviemetase

object App {
  val name    = "MovieMetase"
  val version = "0.1"
  
  def main(args: Array[String]): Unit = {
    
    // fail fast
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
    TaskExecutor.shutdown()
    System.exit(0)
  }
  
  //lazy val Toolkit = java.awt.Toolkit.getDefaultToolkit
  
  def resource(path: String): java.net.URL =
    this.getClass.getResource(path)
  
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