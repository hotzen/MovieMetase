package moviemetase

import scala.io.BufferedSource
import scraping.Scraper

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
      info("loading Scraper " + f.getAbsolutePath)
      val src = scala.io.Source.fromFile(f)
      val cnt = src.getLines.mkString("\n")
      DSL.apply( cnt ) match {
        case DSL.Success(scrapers, _) => scrapers
        case failure => error( failure.toString ); Nil
      }
    })
  }

  def readFile(name: String): BufferedSource = {
    val is = App.resourceStream( "/config/" + name )
    scala.io.Source.fromInputStream(is, "utf-8")
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