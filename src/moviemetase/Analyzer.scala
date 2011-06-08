package moviemetase



case class AnalyzedFile(dir: Chopped, file: Chopped, same: Chopped, all: Chopped) {
  override def toString: String = {
    "AnalyzedFile(\n" +
    "  Dir:  " + dir + "\n" +
    "  File: " + file + "\n" + 
    "  Same: " + same + "\n" +
    "  All:  " + all + ")"
  }
}

object Analyzer {
  
  val SepChars = " ._-,;()[]{}<>#+*" //.toCharArray
    
  lazy val Tags = loadRes("/res/tags.txt")
  lazy val Exts = loadRes("/res/exts.txt")
  
  def analyze(f: FileInfo): AnalyzedFile = {
    val dirParts  = split(f.dirName)
    val fileParts = split(f.fileName)
    val (sameParts, allParts) = getInterUnion(dirParts, fileParts)
    AnalyzedFile(
      dirParts,
      fileParts,
      sameParts,
      allParts
    )
  }
  
  def split(s: String): Chopped = {
    var ps = s.split(SepChars).toList
      .map(_.trim)
      .filter(_.length > 0)
      .map(_.toLowerCase)
    
    var names = List[String]()
    while (!ps.isEmpty && !isYear(ps.head) && !isTag(ps.head)) {
      names = ps.head :: names
      ps = ps.tail
    }
    
    val year = if (!ps.isEmpty && isYear(ps.head)) {
      val yStr = ps.head
      ps = ps.tail
      Some(yStr.toInt)
    } else None
        
    val tags = if (!ps.isEmpty) ps
               else             List[String]()
               
    Chopped(names.reverse, tags, year)
  }
     
  
  // fst = same
  // snd = all
  def getInterUnion(s1: Chopped, s2: Chopped): (Chopped, Chopped) = {
    
    val same = Chopped(
      (s1.names intersect s2.names).distinct,
      (s1.tags intersect s2.tags).distinct,
      s1.year match {
        case Some(y) => {
          s2.year match {
            case Some(y) => Some(y)
            case _       => None
          }
        }
        case None => None
      }
    )
    
    val all = Chopped(
      (s1.names union s2.names).distinct,
      (s1.tags union s2.tags).distinct,
      s1.year match {
        case Some(y) => Some(y)
        case None    => s2.year
      }
    )
    
    (same, all)
  }
    
  
  def isTag(str: String): Boolean =
    Tags.contains(str)
  
  def isYear(s: String): Boolean = {
    if (s.length == 4 && s.toList.forall(_.isDigit)) {
      val year = s.toInt
      year >= 1900 && year <= 2100
    } else if (s.length == 2 && s.toList.forall(_.isDigit)) {
      val year = s.toInt
      year >= 40 && year <= 99
    } else false
  }

  def loadRes(path: String): List[String] = {
    try {
      val f = new java.io.File( this.getClass.getResource(path).toURI )
      scala.io.Source.fromFile(f, "utf-8")
        .getLines
        .map(line => line.trim.toLowerCase)
        .toList
    } catch { case e:Exception => {
      e.printStackTrace()
      Nil
    }}
  }
}
