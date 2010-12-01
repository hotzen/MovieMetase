package moviemetase

case class Query(term: String, year: Option[Int]) {
    
  def year4C: Option[String] = year match {
    case Some(y) if y > 1800 && y < 2100 => Some( y.toString )
    case Some(y) if y <= 99              => Some( "19" + y )
    case _                               => None
  }
}

object QueryGenerator {
  
  type GenFun = AnalyzedFile => Option[Query]
  
  val generators = Seq[GenFun](
      sameName_allYear
    , allName_allYear
    , fileName_allYear
    , dirName_allYear
  )
    
  def sameName_allYear(af: AnalyzedFile): Option[Query] =
    if (af.same.names.isEmpty) None
    else
      Some( Query(af.same.names.mkString(" "), af.all.year) )
    
  def allName_allYear(af: AnalyzedFile): Option[Query] =
    if (af.all.names.isEmpty) None 
    else
      Some( Query(af.all.names.mkString(" "), af.all.year) )
      
      
  def fileName_allYear(af: AnalyzedFile): Option[Query] =
    if (af.file.names.isEmpty) None
    else
      Some( Query(af.file.names.mkString(" "), af.all.year) )
      
  def dirName_allYear(af: AnalyzedFile): Option[Query] =
    if (af.dir.names.isEmpty) None
    else
      Some( Query(af.dir.names.mkString(" "), af.all.year) )
  
  //TODO lazy Stream[Query] ?
  def generate(af: AnalyzedFile): Seq[Query] = 
    generators.flatMap(genFun => genFun(af)).distinct
  
  
}


object QueryManager {

  lazy val googlePool = java.util.concurrent.Executors.newFixedThreadPool(4)
  
  val generators = List
  
  def query(af: AnalyzedFile): Unit = {
    
//    val qts = QueryGenerator.generate( af )
//    
//    println("QueryManager.query AnalyzedFile: " + af)
//    println("QueryManager.query Querys: " + qts.mkString(", "))
//    
//    //TODO search incrementally
//    val qt = qts.head
//    
//    println("QueryManager.query Query: " + qt)
//    
//    val t = qt match {
//      case Query(t, Some(y)) => t + " ("+y+")"
//      case Query(t, None)    => t
//    }
//    
//    val googleSearches = List[GoogleSearch](
//      GoogleSearch( GoogleQuery.Scoped(t, GoogleCSE.MainSites) ),
//      GoogleSearch( GoogleQuery.Scoped(t, GoogleCSE.IMDB) ),
//      GoogleSearch( GoogleQuery.Scoped(t, GoogleCSE.TMDB) ),
//      GoogleSearch( GoogleQuery.Scoped(t, GoogleCSE.Subtitles) )
//    )
//    
//    val googleFutures = googleSearches.map( googlePool.submit(_) ).toList
//        
//    // BLOCKING
//    val resultBuf = new scala.collection.mutable.ListBuffer[GoogleResult]()
//    googleFutures.foreach( fut => resultBuf appendAll fut.get )
//    processGoogleResults( resultBuf.toList )
  }
  
  
//  def createSubtitleSearch(qt: Query): GoogleSearch = {
//    val t = qt match {
//      case Query(t, Some(y)) => t + " ("+y+")"
//      case Query(t, None)    => t
//    }
//
//    GoogleSearch( GoogleQuery.Scoped(t, GoogleCSE.Subtitles) )
//  }
//  
//  def processGoogleResults(rs: List[GoogleResult]): Unit = {
//    
//    println( rs.mkString("\n") )
//    
//    
//  }
  
}