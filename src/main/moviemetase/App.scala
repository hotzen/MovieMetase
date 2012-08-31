package moviemetase

import java.io.IOException

object App {
  val name    = "MovieMetase"
  val version = "0.1"
  
  def main(args: Array[String]): Unit = {
    Config.load()

    if (args.isEmpty) {
      ui.UI.show()
    } else {
      throw new UnsupportedOperationException("no CLI mode yet")
    }
    
    ()
  }
  
  def shutdown() = {
    TaskManager.shutdown()
    System.exit(0)
  }

  def resource(path: String): java.net.URL = {
    val url = this.getClass.getResource(path)
    if (url == null)
      throw new IOException("resource '" + path + "' does not exist")
    url
  }
  
  def resourceStream(path: String): java.io.InputStream = {
    val is = this.getClass.getResourceAsStream(path)
    if (is == null)
      throw new IOException("resource '" + path + "' does not exist")
    is
  }
  
  val userDir: java.io.File = new java.io.File( util.Properties.userHome )

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