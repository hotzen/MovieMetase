package moviemetase

import scala.io.BufferedSource
import scraping.Scraper
import org.omg.CORBA.portable.OutputStream

object Config extends Logging {
  val logID = "Config"
  
  lazy val releaseTags =
    readFile("reltags.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val videoExts =
    readFile("videoexts.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val stopWords =
    readFile("stopwords.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet
  
  lazy val scanExcludes =
    readFile("scanexcl.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val tokenRepl =
    readFile("tokenrepl.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).
      map( _.split("->").map(_.trim) ).
      filter(xs => !xs.isEmpty && !xs.head.isEmpty).
      map(xs => (xs.head, if (xs.tail.isEmpty) "" else xs.tail.head)).toList
  
  lazy val scrapers: List[Scraper[_]] = {
    import java.io._
    import scraping.DSL
    
    val p = new FileFilter {
      def accept(f: File): Boolean = f.getName.endsWith(".scraper")
    }
    val fs = App.dataDir("scraper").listFiles(p).toList
    
    fs.flatMap(f => {
      info("loading " + f.getAbsolutePath)
      val src = scala.io.Source.fromFile(f)
      val cnt = src.getLines.mkString("\n")
      DSL.apply( cnt ) match {
        case DSL.Success(scrapers, _) => scrapers
        case failure => error( failure.toString ); Nil
      }
    })
  }

  def readFile(name: String): BufferedSource = {
    import java.io._
    
    val resPath = "/config/" + name
    
    val configDir = App.dataDir("config")
    val configFile = new File(configDir, name)
    val configPath = configFile.getAbsolutePath
    
    // if config-file does not exist yet, write the bundled resource file to the config-file
    if (!configFile.exists)
      writeResourceToFile(resPath, configPath)
    
    info("loading " + configPath)
      
    val configIS = new FileInputStream(configFile)
      
    // use an additional extra-file if it exists
    val configExtraFile = new File(configDir, name + ".extra")
    val configExtraPath = configExtraFile.getAbsolutePath 
        
    val is =
      if (configExtraFile.exists) {
        info("loading extra " + configExtraPath)
        val configExtraIS = new FileInputStream(configExtraFile)
        new java.io.SequenceInputStream(configIS, configExtraIS)
      } else
        configIS

    scala.io.Source.fromInputStream(is, "UTF-8")
  }
  
  def writeResourceToFile(resPath: String, filePath: String) {
    import java.io._
    import java.nio._
    
    val is = App.resourceStream(resPath)
    val src = channels.Channels.newChannel( is )
    val dest = new FileOutputStream(new File(filePath)).getChannel
    try {
      dest.transferFrom(src, 0, Long.MaxValue)
    } finally {
      src.close()
      dest.close()
    }
  }

  def load() {
    // touch lazy vals
    releaseTags.headOption
    videoExts.headOption
    stopWords.headOption
    scanExcludes.headOption
    tokenRepl.headOption
    scrapers.headOption
  }
}