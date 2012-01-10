package moviemetase

import scala.collection.mutable.ListBuffer
import java.io.File
import scala.util.matching.Regex
import java.nio.file.Path
import java.nio.file.Paths

object Analyzer {
    
// ----------------------------------------------  
  lazy val Tags =
    scala.io.Source.fromFile(App.resource("/res/tags.txt").toURI, "utf-8").
      getLines.map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet

  lazy val Exts =
    scala.io.Source.fromFile(App.resource("/res/exts.txt").toURI, "utf-8").
      getLines.map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet
  
  lazy val SimExcludes =
    scala.io.Source.fromFile(App.resource("/res/simexcl.txt").toURI, "utf-8"). 
      getLines.map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).toSet
  
  lazy val ReplMap =
    scala.io.Source.fromFile(App.resource("/res/repl.txt").toURI, "utf-8").
      getLines.map(_.trim.toLowerCase).filter(x => !x.isEmpty && !x.startsWith("#")).
      map( _.split("->").map(_.trim) ).
      filter(xs => !xs.isEmpty && !xs.head.isEmpty).
      map(xs => (xs.head, if (xs.tail.isEmpty) "" else xs.tail.head)).toList
      
  def isTag(s: String):  Boolean = Tags contains s.toLowerCase
  def isExt(s: String):  Boolean = Exts contains s.toLowerCase


// ----------------------------------------------
  
  def init(): Unit = {
    Tags.head
    Exts.head
    SimExcludes.head
    ReplMap.head
    ()
  }

// ----------------------------------------------
  type Tokens = List[String]
  
  val TokenSplitRegex = """[^a-z0-9]""".r
  
  def tokenize(in: String): Tokens = {
    var s = in.toLowerCase
    
    for ( (a,b) <- ReplMap )
      s = s.replace(a, b)
    
    TokenSplitRegex.split(s).map(_.trim).filter(!_.isEmpty).toList
  }
  
  
//  def fuzzy(in: String): (Regex,Tokens) = {
//    val ts = tokenize(in)
//    val regex = ts.map(t => "(?:" + java.util.regex.Pattern.quote(t) + ")?").mkString(".*?").toString.r
//    (regex, ts)
//  }
  
     
  
// ----------------------------------------------
  def toYear(s: String): Option[Int] = {
    if (!s.toList.forall(_.isDigit))
      return None
        
    if (s.length == 4) {
      val year = s.toInt
      if (year >= 1900 && year <= 2100) Some(year)
      else                              None
    }
    
//    else if (s.length == 2) {
//      val year = s.toInt
//      if (year >= 40 && year <= 99) Some(1900 + year)
//      else                          None
//    }
    
    else None
  }


// ----------------------------------------------
  // Jaccard similarity coefficient
  // http://en.wikipedia.org/wiki/Jaccard_index
  def sim(s1: Iterable[String], s2: Iterable[String]): Float = {
    import scala.collection.immutable.Set
    
    if (s1.isEmpty || s2.isEmpty)
      return 0.0F

    val hs1 = Set[String]() ++ s1
    val hs2 = Set[String]() ++ s2
    
    val i = hs1 intersect hs2
    val u = hs1 union     hs2
    
    (i.size.toFloat / u.size.toFloat)
  }
  
  def sim(s1: String, s2: String): Float = sim(tokenize(s1), tokenize(s2)) 

  
  def dissect(s: String): Dissected = {
    var ts = tokenize(s)
    val names = new ListBuffer[String]
    
    // check last part, if it is an extension ignore it
    if (!ts.isEmpty && Analyzer.isExt(ts.last))
      ts = ts.init
    
    // parts are names until a year or a tag is reached
    while (!ts.isEmpty && !Analyzer.toYear(ts.head).isDefined && !Analyzer.isTag(ts.head)) {
      names append ts.head
      ts = ts.tail
    }
    
    // try to transform the next part to a year
    val year = if (!ts.isEmpty) Analyzer.toYear(ts.head)
               else             None

    // the rest are tags
    val tags = if (year.isDefined) ts.tail
               else                ts

    Dissected(s, names.toList, tags, year)
  }
  
  def dissectFileInfo(info: FileInfo): DissectedFileInfo = {
    val dir  = Analyzer dissect info.dirName
    val file = Analyzer dissect info.fileNameWithoutExt

    DissectedFileInfo(info, dir, file)
  }
}


object FileInfo {
  def apply(f: File): FileInfo      = FileInfo(f.getAbsolutePath, f.getParentFile.getName, f.getName)
  def apply(p: Path): FileInfo      = apply( p.toFile )
  def apply(path: String): FileInfo = apply( new File(path) )
}


case class FileInfo(path: String, dirName: String, fileName: String) {
  
  lazy val fileExt: String = {
    val pos = fileName.lastIndexOf(".")
    if (pos < 0)
      ""
    else
      fileName.substring(pos+1).toLowerCase
  }
  
  lazy val fileNameWithoutExt: String = {
    val pos = fileName.lastIndexOf(".")
    if (pos < 0)
      fileName
    else
      fileName.substring(0, pos)
  }
  
  override def toString: String = path
  
//  override def toString: String = {
//    val sb = new StringBuffer("FileInfo(")
//    sb append path
//    sb append ")"
////    sb append "){\n"
////    sb append "  path: " append path append "\n"
////    sb append "}"
//    sb.toString
//  }
}


object Dissected {
  def same(d1: Dissected, d2: Dissected): Dissected =
    Dissected(
      d1.orig + " & " + d2.orig,
      (d1.names intersect d2.names).distinct,
      (d1.tags  intersect d2.tags).distinct,
      (d1.year, d2.year) match {
        case (Some(a), Some(b)) if a == b => Some(a)
        case _                            => None
      }
    )

  def all(d1: Dissected, d2: Dissected): Dissected = 
    Dissected(
      d1.orig + " | " + d2.orig,
      (d1.names union d2.names).distinct,
      (d1.tags  union d2.tags).distinct,
      (d1.year, d2.year) match {
        case (Some(a), _) => Some(a)
        case (_,       b) => b
      }
    )
  
  // XXX remove?
//  def simExact(d1: Dissected, d2: Dissected, exact: Boolean = true): (Float, Float) = {
//    val simNames = Analyzer.sim(d1.names, d2.names)
//    val simTags  = Analyzer.sim(d1.tags, d2.tags)
//    (simNames, simTags)
//  }
  
  def sim(d1: Dissected, d2: Dissected, exact: Boolean = true): (Float, Float) = {
    val simNames = Analyzer.sim(
      d1.names.filter(!Analyzer.SimExcludes.contains(_)),
      d2.names.filter(!Analyzer.SimExcludes.contains(_))
    )
    val simTags  = Analyzer.sim(d1.tags, d2.tags)
    (simNames, simTags)
  }
}


case class Dissected(orig: String, names: List[String], tags: List[String], year: Option[Int]) {
  def name: String = names.mkString(" ")
  def tokens: List[String] = Analyzer.tokenize( orig )
  
  def same(d2: Dissected) = Dissected.same(this, d2)
  def all(d2: Dissected)  = Dissected.all(this, d2)
  def sim(d2: Dissected)  = Dissected.sim(this, d2)
  
  override def toString: String = {
    val sb = new StringBuffer("Dissected(")
    sb append names.mkString("[", ", ", "]")
    sb append " / " append year append " / "
    sb append tags.mkString("[", ", ", "]")
    sb append " {"
    sb append orig
    sb append "})"
    sb.toString
  }
}

case class DissectedFileInfo(info: FileInfo, dir: Dissected, file: Dissected) {
  
  lazy val same = Dissected.same(dir, file)
  lazy val all  = Dissected.all(dir, file)
  lazy val sim  = Dissected.sim(dir, file)
  
  override def toString: String = {
    val sb = new StringBuffer("DissectedFileInfo(")
    sb append info.toString
    sb append "){\n"
    sb append "  dir:  " append dir.toString  append "\n"
    sb append "  file: " append file.toString append "\n"
    sb append "  same: " append same.toString append "\n"
    sb append "  all:  " append all.toString  append "\n"
    sb append "  sim:  " append "(names: " + sim._1 append ", tags: " append sim._2 append ")\n"
    sb append "}"
    sb.toString
  }
}

// TODO
//object MediaInfo {
//  import nu.xom._
//  import XOM._
//  
//  def apply(doc: Document): MediaInfo = {
//    // MediaInfo --Inform=General,Video,Audio,Text --Output=XML "...mkv"
//    null
//  }
//
//}
//
//case class MediaInfo(foo: String, bar: String)
//
