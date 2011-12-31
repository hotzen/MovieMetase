package moviemetase
import java.io.File


object Analyzer {


// ----------------------------------------------  
  //val SepChars = """ ._-,;()[]{}<>#+*"'"""
  //s.split(SepChars).map(_.trim).filter(_.length > 0).map(_.toUpperCase).toList
  
  val SplitRegex = """[^A-Z0-9ÄÖÜ]""".r
    
  def split(s: String): List[String] =
    SplitRegex.split( s.toLowerCase ).map(_.trim).filter(!_.isEmpty).toList
    
  

  
// ----------------------------------------------  
  lazy val Tags = loadRes("/res/tags.txt")
  lazy val Exts = loadRes("/res/exts.txt")
    
  def isTag(s: String): Boolean = Tags contains s.toLowerCase
  def isExt(s: String): Boolean = Exts contains s.toLowerCase

  
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
   
  
    
//DissectedFileInfo( V:\01_Filme\Dark.Knight.1080p.BluRay.x264-HD1080\hd1080-tdk.mkv
//  Dir:  Dissected( [DARK, KNIGHT] None {1080P, BLURAY, X264, HD1080} )
//  File: Dissected( [HD1080, TDK, MKV] None {} )
//  Same: Dissected( [] None {} )
//  All:  Dissected( [DARK, KNIGHT, HD1080, TDK, MKV] None {1080P, BLURAY, X264, HD1080} )
//)
  
//  val MovieRules = List[DissectedFileInfo => Option[Movie]](
//      
//      
//      (dfi => None)
//  )
//  
  
  def guessMovieFromFile(dfi: DissectedFileInfo): Option[Movie] = {
    
    val infos = new scala.collection.mutable.ListBuffer[MovieInfo]
    
    if (!dfi.same.names.isEmpty) {
      val sameToDirRatio  = dfi.same.names.size / dfi.dir.names.size
      val sameToFileRatio = dfi.same.names.size / dfi.file.names.size
      val RatioThreshold  = 0.7
      
      // use same names if they are about 70% of both dir-names and file-names
      if (sameToDirRatio >= RatioThreshold && sameToFileRatio >= RatioThreshold) {
        infos append MovieInfos.Title( dfi.same.names.mkString(" ") )
      } else {
        
      }
    } else {
      
    }
    
    if (dfi.same.year.isDefined)
      infos append MovieInfos.Release( dfi.same.year.get )
    else if (dfi.all.year.isDefined)
      infos append MovieInfos.Release( dfi.all.year.get )
    
    Movie( infos.toList )
  }
  
  
  
// ----------------------------------------------
    
  //TODO: refactor to other place, sucks here
  def loadRes(path: String): Set[String] = {
    try {
      val uri = this.getClass.getResource(path).toURI
      val f   = new java.io.File( uri )
      
      scala.io.Source.fromFile(f, "utf-8")
        .getLines.map(line => line.trim.toLowerCase)
        .toSet
        
    } catch { case e:Exception => {
      e.printStackTrace()
      Set.empty
    }}
  }
}



object Dissected {
  def apply(s: String): Dissected = {
    var ps = Analyzer.split(s)
    
    // parts are names until a year or a tag is reached
    var names = new scala.collection.mutable.ListBuffer[String]
    
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

    Dissected(names.toList, tags, year)
  }
    
   
  def intersect(d1: Dissected, d2: Dissected): Dissected =
    Dissected(
      (d1.names intersect d2.names).distinct,
      (d1.tags  intersect d2.tags).distinct,
      (d1.year, d2.year) match {
        case (Some(a), Some(b)) if a == b => Some(a)
        case _                            => None
      }
    )
    
  def union(d1: Dissected, d2: Dissected): Dissected = 
    Dissected(
      (d1.names union d2.names).distinct,
      (d1.tags  union d2.tags).distinct,
      (d1.year, d2.year) match {
        case (Some(a), _) => Some(a)
        case (_,       b) => b
      }
    )
  
  def sim(d1: Dissected, d2: Dissected): (Float, Float) =
    ( Analyzer.sim(d1.names, d2.names)
    , Analyzer.sim(d1.tags,  d2.tags)  )
}


case class Dissected(names: List[String], tags: List[String], year: Option[Int]) {
  def name: String = names.mkString(" ")
  def tag:  String = tags.mkString(" ")
  
  def intersect(d2: Dissected) = Dissected.intersect(this, d2)
  def union(d2: Dissected)     = Dissected.union(this, d2)
  
  def sim(d2: Dissected) = Dissected.sim(this, d2)
  
  override def toString: String = {
    "Dissected( [" + names.mkString(", ") + "] " +
                year + " " + 
                "{" + tags.mkString(", ") + "} )"
  }
}

object FileInfo {
  def apply(f: File): FileInfo =
    FileInfo(f.getAbsolutePath, f.getParentFile.getName, f.getName)
    
  def apply(path: String): FileInfo = apply( new File(path) )
}
case class FileInfo(path: String, dirName: String, fileName: String) {
  override def toString: String = {
    "FileInfo( " + path + "\n" + 
    "  DirName:  " + dirName + "\n" + 
    "  FileName: " + fileName + "\n)"
  }
}


object DissectedFileInfo {
  def apply(info: FileInfo): DissectedFileInfo = {
    val dir  = Dissected(info.dirName)
    val file = Dissected(info.fileName)
    DissectedFileInfo(
      info,
      dir,
      file,
      Dissected.intersect(dir, file),
      Dissected.union(dir, file)
    )
  }
}

case class DissectedFileInfo(info: FileInfo, dir: Dissected, file: Dissected, same: Dissected, all: Dissected) {
  override def toString: String = {
    "DissectedFileInfo( " + info.path + "\n" + 
    "  Dir:  " + dir + "\n" +
    "  File: " + file + "\n" + 
    "  Same: " + same + "\n" +
    "  All:  " + all + "\n)"
  }
}