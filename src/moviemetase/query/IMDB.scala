package moviemetase.query

object IMDB {
  val CSE = "011282045967305256347:dyc6spozqnc"
    
  val TitleUrlRegex  = """imdb.com/title/tt[0-9]+""".r
  val TitlePathRegex = """/title/tt[0-9]+""".r
  val IdRegex   = """tt[0-9]+""".r
  
  def extractTitleUrls(s: String): List[String] = TitleUrlRegex.findAllIn( s ).map( m => "http://www." + m + "/").toList

  def extractTitlePaths(s: String): List[String] = TitlePathRegex.findAllIn( s ).toList
    
  def extractIds(s: String): List[String] = IdRegex.findAllIn( s ).toList
    
  def extractId(s: String): Option[String] = extractIds(s).headOption
}
