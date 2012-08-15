package moviemetase

import scala.io.BufferedSource

object Config {
  lazy val tags =
    readFile("tags.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val exts =
    readFile("exts.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val simExcludes =
    readFile("simexcl.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet
  
  lazy val scanExcludes =
    readFile("scanexcl.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val replMap =
    readFile("repl.txt").getLines.
      map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).
      map( _.split("->").map(_.trim) ).
      filter(xs => !xs.isEmpty && !xs.head.isEmpty).
      map(xs => (xs.head, if (xs.tail.isEmpty) "" else xs.tail.head)).toList
  
  def readFile(name: String): BufferedSource = {
    val is = App.resourceStream( "/config/" + name )
    scala.io.Source.fromInputStream(is, "utf-8")
  }
      
      
  def load() {
    tags.head
    exts.head
    simExcludes.head
    scanExcludes.head
    replMap.head
    
  }
}