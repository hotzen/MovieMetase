package moviemetase

import scala.collection.mutable.ListBuffer
import java.io.File

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
  val SplitRegex = """[^a-z0-9]""".r
  
  type SplitString = List[String]
  def split(in: String): SplitString = {
    var out = in.toLowerCase
    
    for ( (s,r) <- ReplMap )
      out = out.replace(s, r)
    
    SplitRegex.split( out ).map(_.trim).filter( !_.isEmpty ).toList
  }
  
     
  
// ----------------------------------------------
  def toYear(s: String): Option[Int] = {
    if (s.length == 4 && s.toList.forall(_.isDigit)) {
      val year = s.toInt
      if (year >= 1900 && year <= 2100) Some(year)
      else                              None
        
    } else if (s.length == 2 && s.toList.forall(_.isDigit)) {
      val year = s.toInt
      if (year >= 40 && year <= 99) Some(1900 + year)
      else                          None
    
    } else None
  }


// ----------------------------------------------
  // Jaccard similarity coefficient
  // http://en.wikipedia.org/wiki/Jaccard_index
  def sim(s1: Iterable[String], s2: Iterable[String]): Float = {
    import scala.collection.immutable.HashSet
    
    if (s1.isEmpty || s2.isEmpty)
      return 0.0F

    val hs1 = HashSet[String]() ++ s1
    val hs2 = HashSet[String]() ++ s2
    
    val i = hs1 intersect hs2
    val u = hs1 union     hs2
    
    (i.size.toFloat / u.size.toFloat)
  }
  
  def sim(s1: String, s2: String): Float = sim(split(s1), split(s2)) 

  
// ----------------------------------------------
  def dissect(s: String): Dissected = Dissected(s)
  def dissectFileInfo(info: FileInfo): DissectedFileInfo = DissectedFileInfo(info)
  
//    
////DissectedFileInfo( V:\01_Filme\Dark.Knight.1080p.BluRay.x264-HD1080\hd1080-tdk.mkv
////  Dir:  Dissected( [DARK, KNIGHT] None {1080P, BLURAY, X264, HD1080} )
////  File: Dissected( [HD1080, TDK, MKV] None {} )
////  Same: Dissected( [] None {} )
////  All:  Dissected( [DARK, KNIGHT, HD1080, TDK, MKV] None {1080P, BLURAY, X264, HD1080} )
////)
//  
////  val MovieRules = List[DissectedFileInfo => Option[Movie]](
////      
////      
////      (dfi => None)
////  )
////  
//  
//  def guessMovieFromFile(dfi: DissectedFileInfo): Option[Movie] = {
//    
//    val infos = new scala.collection.mutable.ListBuffer[MovieInfo]
//    
//    if (!dfi.same.names.isEmpty) {
//      val sameToDirRatio  = dfi.same.names.size / dfi.dir.names.size
//      val sameToFileRatio = dfi.same.names.size / dfi.file.names.size
//      val RatioThreshold  = 0.7
//      
//      // use same names if they are about 70% of both dir-names and file-names
//      if (sameToDirRatio >= RatioThreshold && sameToFileRatio >= RatioThreshold) {
//        infos append MovieInfos.Title( dfi.same.names.mkString(" ") )
//      } else {
//        
//      }
//    } else {
//      
//    }
//    
//    if (dfi.same.year.isDefined)
//      infos append MovieInfos.Release( dfi.same.year.get )
//    else if (dfi.all.year.isDefined)
//      infos append MovieInfos.Release( dfi.all.year.get )
//    
//    Movie( infos.toList )
//  }
}


object FileInfo {
  def apply(f: File): FileInfo = FileInfo(f.getAbsolutePath, f.getParentFile.getName, f.getName)
    
  def apply(path: String): FileInfo = apply( new File(path) )
}

case class FileInfo(path: String, dirName: String, fileName: String) {
  override def toString: String = {
    val sb = new StringBuffer("FileInfo(")
    sb append path
    sb append ")"
//    sb append "){\n"
//    sb append "  path: " append path append "\n"
//    sb append "}"
    sb.toString
  }
}


object Dissected {
  def apply(s: String): Dissected = {
    var ps = Analyzer.split(s)
    val names = new ListBuffer[String]
    
    // check last part, if it is an extension ignore it
    if (!ps.isEmpty && Analyzer.isExt(ps.last))
      ps = ps.init
    
    // parts are names until a year or a tag is reached
    while (!ps.isEmpty && !Analyzer.toYear(ps.head).isDefined && !Analyzer.isTag(ps.head)) {
      names append ps.head
      ps = ps.tail
    }
    
    // try to transform the next part to a year
    val year = if (!ps.isEmpty) Analyzer.toYear(ps.head)
               else             None

    // the rest are tags
    val tags = if (year.isDefined) ps.tail
               else                ps

    Dissected(s, names.toList, tags, year)
  }
  
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
  def simExact(d1: Dissected, d2: Dissected, exact: Boolean = true): (Float, Float) = {
    val simNames = Analyzer.sim(d1.names, d2.names)
    val simTags  = Analyzer.sim(d1.tags, d2.tags)
    (simNames, simTags)
  }
  
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
  def parts: List[String] = Analyzer.split( orig )
  
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


object DissectedFileInfo {
  //def apply(info: FileInfo): DissectedFileInfo = DissectedFileInfo(info, Dissected(info.dirName), Dissected(info.fileName) )
}

case class DissectedFileInfo(info: FileInfo) {

  val dir  = Dissected(info.dirName)
  val file = Dissected(info.fileName)
  
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
object MediaInfo {
  import nu.xom._
  import XOM._
  
  def apply(doc: Document): MediaInfo = {
    // MediaInfo --Inform=General,Video,Audio,Text --Output=XML "...mkv"
    null
  }

}

case class MediaInfo(foo: String, bar: String)

