package moviemetase
package search

import sites._
import java.net.URL
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream
import scala.util.matching.Regex


class MovieSearch() extends SearchManager[Movie] with Logging {
  val logID = "MovieSearch"
    
  def searchByTerm(term: String): List[Movie] = {
    val movies = new ListBuffer[Movie]
    
    val dis = Analyzer.dissect(term)
    trace(dis.toString)
    
    if (movies.isEmpty && !dis.tags.isEmpty) {
      trace("Exact-Term linking to IMDB")
      
      val t = "\"" + term + "\""
      movies appendAll GoogleTermWithImdbLink(t, Some(resultMustContain(dis.tokens))).execute().filter(postFilter(dis, _))
    }
    
    if (movies.isEmpty && !dis.names.isEmpty) {
      trace("Exact-Name & Any Year & Any Tags search linking to IMDB")
      
      val t1 = "\"" + dis.name + "\""
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink(t3).execute().filter(postFilter(dis, _))
    }
    
    if (movies.isEmpty && !dis.names.isEmpty) {
      trace("Name & Any Year search at IMDB")
      
      val t1 = dis.name
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.tags.mkString(" ")
      movies appendAll GoogleTermAtImdbSite( t3 ).execute().filter(postFilter(dis, _))
    }
            
    // NOTHING FOUND
    if (movies.isEmpty) {
      warn("no results for " + term)
      return Nil
    }

    // auto-complete with TMDB
    movies.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
  }
  
  def searchByFile(fileInfo: FileInfo): List[Movie] = {
    val movies = new ListBuffer[Movie]
    
    val dis = Analyzer.dissectFileInfo(fileInfo)
    trace(dis.toString)

    val beginWithFileName =
      if (dis.file.tokens.length > dis.dir.tokens.length) true
      else false
    
    def searchExactFileName() = {
      if (movies.isEmpty) {
        trace("Exact-FileName linking to IMDB")
        
        val fileName = fileInfo.fileName //WithoutExt
        val t = "\"" + fileName + "\""
        movies appendAll GoogleTermWithImdbLink(t, Some(resultMustContain(dis.file.tokens) _)).execute().filter(postFilter(dis, _))
      }
    }
    
    def searchExactDirName() = {
      if (movies.isEmpty) {
        trace("Exact-DirName linking to IMDB")
        
        val dirName = fileInfo.dirName
        val t = "\"" + dirName + "\""
        movies appendAll GoogleTermWithImdbLink(t, Some(resultMustContain(dis.dir.tokens) _)).execute().filter(postFilter(dis, _))
      }
    }
    
    if (beginWithFileName) {
      searchExactFileName()
      searchExactDirName()
    } else {
      searchExactDirName()
      searchExactFileName()
    }
    
    if (movies.isEmpty && !dis.all.names.isEmpty && !dis.all.tags.isEmpty) {
      trace("All Names & Any Year & All Tags linking to IMDB")
      
      val t1 = dis.all.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.all.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute().filter(postFilter(dis, _))
    }
    
    if (movies.isEmpty && !dis.same.names.isEmpty && !dis.same.tags.isEmpty) {
      trace("Same Names & Any Year & Same Tags linking to IMDB")
      
      val t1 = dis.same.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.same.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute().filter(postFilter(dis, _))
    }
    
    if (movies.isEmpty && !dis.file.names.isEmpty && !dis.file.tags.isEmpty) {
      trace("File Names & Any Year & File Tags linking to IMDB")
      
      val t1 = dis.file.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.file.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute().filter(postFilter(dis, _))
    }
    
    if (movies.isEmpty && !dis.dir.names.isEmpty && !dis.dir.tags.isEmpty) {
      trace("Dir Names & Any Year & Dir Tags linking to IMDB")
      
      val t1 = dis.dir.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.dir.tags.mkString(" ")
      movies appendAll GoogleTermWithImdbLink( t3 ).execute().filter(postFilter(dis, _))
    }
    
    // NOTHING FOUND
    if (movies.isEmpty) {
      warn("no results for " + fileInfo)
      return Nil
    }

    // auto-complete with TMDB
    movies.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
  }
  
  def resultMustContain(reqTokens: List[String])(res: GoogleResult): Boolean = {
    val resTokens = Analyzer.tokenize(res.title) ::: Analyzer.tokenize(res.snippet)
    val allContained = reqTokens.forall( resTokens contains _ )
    
    if (!allContained) {
      trace("IGNORE " + res + "\n  required:  [" + reqTokens.mkString(", ") + "]:\n  result  : [" + resTokens.mkString(", ") + "]")
    }
    allContained
  }
  
  def postFilter(dis: Dissected, movie: Movie): Boolean = {
    
    if (dis.year.isDefined && (movie.year == 0 || movie.year != dis.year.get)) {
      trace("IGNORE " + movie + " because it specifies different year than " + dis)
      return false
    }
    
    true
  }
  
  def postFilter(dis: DissectedFileInfo, movie: Movie): Boolean = {
    
    if (dis.all.year.isDefined && (movie.year == 0 || movie.year != dis.all.year.get)) {
      trace("IGNORE " + movie + " because it specifies different year than " + dis)
      return false
    }
    
    true
  }
  
//  def resultMustContain(ss: List[String])(res: GoogleResult): Boolean = {
//    val title   = Analyzer.tokenize(res.title).mkString("")
//    val snippet = Analyzer.tokenize(res.snippet).mkString("")
//    
//    val search  = ss.map(Analyzer.tokenize(_).mkString(""))
//
//    search.exists(s => title.contains(s) || snippet.contains(s))
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