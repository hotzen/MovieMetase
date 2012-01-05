package moviemetase
package search

import sites._
import java.net.URL
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream
import scala.util.matching.Regex


case class MovieSearch(val out: PrintStream = System.out) extends SearchManager[Movie] with Logging {
  val logID = "MovieSearch"
  
  Logging.out = out
  
  def searchByTerm(term: String): List[Movie] = {
    val movies = new ListBuffer[Movie]
    
    val dis = Dissected(term)
    trace(dis.toString)
    
    if (movies.isEmpty && !dis.tags.isEmpty) {
      trace("Exact-Term search...")
      
      val t = "\"" + term + "\""
      movies appendAll GoogleTermWithImdbLink(t, Some(googleMatchIt(term))).execute()
    }
    
    if (movies.isEmpty && !dis.names.isEmpty && !dis.tags.isEmpty) {
      trace("Exact-Name & Any Year & Tags search ...")
      
      val t1 = "\"" + dis.name + "\""
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink(t3).execute()
    }
    
    if (movies.isEmpty && !dis.names.isEmpty) {
      trace("Exact-Name & Any Year search ...")
      
      val t1 = "\"" + dis.name + "\""
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      movies appendAll GoogleTermAtImdbSite( t2 ).execute()
    }
    
    if (movies.isEmpty && !dis.names.isEmpty) {
      trace("Name & Any Year search ...")
      
      val name = dis.name
      val t = dis.year match {
        case Some(year) => name + " " + year
        case None       => name
      }
      movies appendAll GoogleTermAtImdbSite( t ).execute()
    }
    
    if (movies.isEmpty) {
      trace("last resort, full search ...")
      
      val t = dis.parts.mkString(" ")
      movies appendAll GoogleTermAtImdbSite( t ).execute()
    }
        
    // NOTHING FOUND
    if (movies.isEmpty) {
      warn("Nothing found")
      return Nil
    }
      
        
    // auto-complete with TMDB
    movies.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
  }
  
  def searchByFile(fileInfo: FileInfo): List[Movie] = {
    val movies = new ListBuffer[Movie]
    
    val dis = DissectedFileInfo(fileInfo)
    trace(dis.toString)
    
    val GoodSim = 0.7
    val FairSim = 0.5
    
    if (movies.isEmpty && !dis.file.tags.isEmpty && !dis.dir.tags.isEmpty) {
      trace("Exact-FileName OR Exact-DirName search...")
      
      val fileName = fileInfo.fileNameWithoutExt
      val t1 = "\"" + fileName + "\""
      
      val dirName = fileInfo.dirName
      val t2 = "\"" + dirName + "\""
      
      val t3 = t1 + " OR " + t2

      movies appendAll GoogleTermWithImdbLink(t3, Some(googleMatchAny(fileName :: dirName :: Nil) _)).execute()
    }
    
    if (movies.isEmpty && !dis.dir.tags.isEmpty) {
      trace("Exact-FileName search...")

      val fileName = fileInfo.fileNameWithoutExt
      val t = "\"" + fileName + "\""
      movies appendAll GoogleTermWithImdbLink(t, Some(googleMatchIt(fileName))).execute()
    }
    
    if (movies.isEmpty && !dis.dir.tags.isEmpty) {
      trace("Exact-DirName search...")
       
      val dirName = fileInfo.dirName
      val t = "\"" + dirName + "\""
      movies appendAll GoogleTermWithImdbLink(t, Some(googleMatchIt(dirName))).execute()
    }
    
    if (movies.isEmpty && !dis.all.names.isEmpty && !dis.all.tags.isEmpty) {
      trace("All Names & Any Year & All Tags search...")
      
      val t1 = dis.all.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.all.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute()
    }
    
    if (movies.isEmpty && !dis.same.names.isEmpty && !dis.same.tags.isEmpty /* && dis.sim._1 >= GoodSim && dis.sim._2 >= GoodSim */) {
      trace("Same Names & Any Year & Same Tags search...")
      
      val t1 = dis.same.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.same.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute()
    }
    
    if (movies.isEmpty && !dis.same.names.isEmpty && !dis.same.tags.isEmpty /* && dis.sim._1 >= GoodSim */) {
      trace("Same Names & Any Year & All Tags search...")
      
      val t1 = dis.same.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.all.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute()
    }
    
    if (movies.isEmpty && !dis.all.names.isEmpty) {
      trace("All Names & Any Year search...")
      
      val t1 = dis.all.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      movies appendAll GoogleTermAtImdbSite( t2 ).execute()
    }
    
    if (movies.isEmpty && !dis.same.names.isEmpty && dis.sim._1 >= GoodSim) {
      trace("Same Names & Any Year search...")
      
      val t1 = dis.same.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      movies appendAll GoogleTermAtImdbSite( t2 ).execute()
    }
    
    
    // NOTHING FOUND
    if (movies.isEmpty) {
      warn("Nothing found")
      return Nil
    }
          
    // auto-complete with TMDB
    movies.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
  }
  
  def googleMatchIt(s: String)(res: GoogleResult): Boolean = googleMatchAny(s :: Nil)(res)
  
  def googleMatchAny(ss: List[String])(res: GoogleResult): Boolean = {
    val title   = Analyzer.tokenize(res.title).mkString("")
    val snippet = Analyzer.tokenize(res.snippet).mkString("")
    
    val search  = ss.map(Analyzer.tokenize(_).mkString(""))

    search.exists(s => title.contains(s) || snippet.contains(s))
  }
  
//  def googleMatchAny(ss: List[String])(res: GoogleResult): Boolean = {
//    val title   = res.title.toLowerCase //Analyzer.split(res.title).mkString("")
//    val snippet = res.snippet.toLowerCase // Analyzer.split(res.snippet).mkString("")
//    //ss.map(Analyzer.split(_).mkString("")).exists(s => title.contains(s) || snippet.contains(s))
//    
//    ss.exists(s => {
//      val (regex, tokens) = Analyzer.fuzzy(s)
//      
//      println("===")
//      println("searching for " + s)
//      println("tokens: " + tokens)
//      println("regex:  " + regex)
//      println()
//      println("title:   " + title)
//      println("snippet: " + snippet)
//      println()
//      
//      regex.findFirstIn(title).orElse( regex.findFirstIn(snippet) ) match {
//        case Some(m) if !m.isEmpty => {
//          val matchTokens = Analyzer.tokenize(m)
//          val sim = Analyzer.sim(tokens, matchTokens)
//          
//          println("match:        " + tokens)
//          println("match-tokens: " + matchTokens)
//          println("sim: " + sim)
//          println()
//          
//          false
//        }
//        case _ => false 
//      }
//    })
//  }
  

        
//  def sortMovies(ms: List[Movie]): List[Movie] =
//    ms.sortWith( (m1,m2) => {
//      val scores1 = m1.infos.collect({ case MovieInfos.Score(score) => score})
//      val scores2 = m2.infos.collect({ case MovieInfos.Score(score) => score})
//      
//      val s1 = if (scores1.isEmpty) 0.3
//               else scores1.max
//      
//      val s2 = if (scores2.isEmpty) 0.3
//               else scores2.max
//      
//      s1 <= s2
//    })
}