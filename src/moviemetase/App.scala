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
    println("shutting down ...")
    TaskManager.shutdown()
    System.exit(0)
  }

  def resource(path: String): java.net.URL = {
    val url = this.getClass.getResource(path)
    if (url == null)
      throw new Exception("resource '" + path + "' does not exist")
    url
  }

  val userDir: java.io.File = new java.io.File( util.Properties.userHome )
    
//  def resourceFromUserDirectory(path: String): java.io.File =
//    new java.io.File(userDir, path)
  
  def dataDir(subDirName: String = ""): java.io.File = {
    val baseDir = new java.io.File(userDir, "MovieMetase")
      
    if (!baseDir.exists)
      baseDir.mkdir()
    
    if (subDirName.isEmpty)
      return baseDir
    
    val dataDir = new java.io.File(baseDir, subDirName)
    if (!dataDir.exists)
      dataDir.mkdir()
    
    dataDir
  }
}