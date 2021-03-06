package moviemetase
package search

import java.net.URL
import scala.collection.mutable.ListBuffer
import sites._
import Util._

class SubtitleSearch() extends SearchManager[MovieInfos.Subtitle] with Logging {
  val logID = "SubtitleSearch"
  
  def searchByTerm(term: String): List[MovieInfos.Subtitle] = {
    val subs = new ListBuffer[MovieInfos.Subtitle]
    
    val dis = Analyzer.dissect(term)
    trace(dis.toString)
    
    // contains tags, EXACT search
    if (subs.isEmpty && !dis.tags.isEmpty) {
      trace("SearchStrategy: Exact-Term linking to IMDB")
      
      val t = "\"" + term + "\""
      val f1 = containsAllTokensFilter(dis.tokens) _
      val f2 = satisfiesDissectedInfos(dis) _
      
      //subs appendAll GoogleTermLinkingToImdb(t, Some(f1)).execute().filter(f2)
    }
    
    // contains tags, search for the EXACT name, year and use tag-hints
    if (subs.isEmpty && !dis.names.isEmpty) {
      trace("SearchStrategy: Exact-Name & Any Year & Any Tags search linking to IMDB")
      
      val t1 = "\"" + dis.name + "\""
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.tags.mkString(" ")
      
      val f = satisfiesDissectedInfos(dis) _
      
      //subs appendAll GoogleTermLinkingToImdb(t3).execute().filter(f)
    }
    
    // contains name, search for the name and year
    if (subs.isEmpty && !dis.names.isEmpty) {
      trace("SearchStrategy: Name & Any Year search at IMDB")
      
      val t1 = dis.name
      val t2 = dis.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.tags.mkString(" ")
      
      val f = satisfiesDissectedInfos(dis) _
      
      //subs appendAll GoogleTermAtImdbSite( t3 ).execute().filter(f)
    }
            
    // NOTHING FOUND
    if (subs.isEmpty) {
      warn("no results for " + term)
      return Nil
    }
    
    //subs.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
    //subs.toList
    Nil
  }
  
  def searchByFile(fileInfo: FileInfo): List[MovieInfos.Subtitle] = {
    val subs = new ListBuffer[Movie]
    
    val dis = Analyzer.dissectFileInfo(fileInfo)
    trace(dis.toString)

    val beginWithFileName =
      if (dis.file.tokens.length > dis.dir.tokens.length) true
      else false
    
    def searchExactFileName() = {
      if (subs.isEmpty) {
        trace("SearchStrategy: Exact-FileName linking to IMDB")
        
        val fileName = fileInfo.fileNameWithoutExt
        val t = "\"" + fileName + "\""
        
        val f1 = containsAllTokensFilter(dis.file.tokens) _
        val f2 = satisfiesDissectedFileInfos(dis) _
        
        //subs appendAll GoogleTermLinkingToImdb(t, Some(f1)).execute().filter(f2)
      }
    }
    
    def searchExactDirName() = {
      if (subs.isEmpty) {
        trace("SearchStrategy: Exact-DirName linking to IMDB")
        
        val dirName = fileInfo.dirName
        val t = "\"" + dirName + "\""
        
        val f1 = containsAllTokensFilter(dis.dir.tokens) _
        val f2 = satisfiesDissectedFileInfos(dis) _
        
        //subs appendAll GoogleTermLinkingToImdb(t, Some(f1)).execute().filter(f2)
      }
    }
    
    if (beginWithFileName) {
      searchExactFileName()
      searchExactDirName()
    } else {
      searchExactDirName()
      searchExactFileName()
    }
    
    if (subs.isEmpty && !dis.all.names.isEmpty && !dis.all.tags.isEmpty) {
      trace("SearchStrategy: All Names & Any Year & All Tags linking to IMDB")
      
      val t1 = dis.all.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.all.tags.mkString(" ")
      
      val f = satisfiesDissectedFileInfos(dis) _
      
      subs appendAll GoogleTermLinkingToImdb( t3 ).execute().filter(f)
    }
    
    if (subs.isEmpty && !dis.same.names.isEmpty && !dis.same.tags.isEmpty) {
      trace("SearchStrategy: Same Names & Any Year & Same Tags linking to IMDB")
      
      val t1 = dis.same.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.same.tags.mkString(" ")
      
      val f = satisfiesDissectedFileInfos(dis) _
      
      subs appendAll GoogleTermLinkingToImdb( t3 ).execute().filter(f)
    }
    
    if (subs.isEmpty && !dis.file.names.isEmpty && !dis.file.tags.isEmpty) {
      trace("SearchStrategy: File Names & Any Year & File Tags linking to IMDB")
      
      val t1 = dis.file.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.file.tags.mkString(" ")
      
      val f = satisfiesDissectedFileInfos(dis) _
      
      subs appendAll GoogleTermLinkingToImdb( t3 ).execute().filter(f)
    }
    
    if (subs.isEmpty && !dis.dir.names.isEmpty && !dis.dir.tags.isEmpty) {
      trace("SearchStrategy: Dir Names & Any Year & Dir Tags linking to IMDB")
      
      val t1 = dis.dir.name
      val t2 = dis.all.year match {
        case Some(year) => t1 + " " + year
        case None       => t1
      }
      val t3 = t2 + " " + dis.dir.tags.mkString(" ")
      
      val f = satisfiesDissectedFileInfos(dis) _
      
      subs appendAll GoogleTermLinkingToImdb( t3 ).execute().filter(f)
    }
    
    // NOTHING FOUND
    if (subs.isEmpty) {
      warn("All SearchStrategies failed, no results for " + fileInfo)
      return Nil
    }

    // auto-complete with TMDB
    //subs.map( TMDB.AutoExpandMovie(_).submit() ).map(_.get()).toList
    Nil
  }
  
  def containsAllTokensFilter(reqTokens: List[String])(res: GoogleResult): Boolean = {
    val resTokens = Analyzer.tokenize(res.title) ::: Analyzer.tokenize(res.snippet)
    val allContained = reqTokens.forall( resTokens contains _ )
    
    if (!allContained) {
      trace("IGNORE " + res + "\n  required:  [" + reqTokens.mkString(", ") + "]:\n  result  : [" + resTokens.mkString(", ") + "]")
    }
    
    allContained
  }
  
  def satisfiesDissectedInfos(dis: Dissected)(movie: Movie): Boolean = {
    
    if (dis.year.isDefined && (movie.year == 0 || movie.year != dis.year.get)) {
      trace("IGNORE " + movie + " because it specifies different year than " + dis)
      return false
    }
    
    true
  }
  
  def satisfiesDissectedFileInfos(dis: DissectedFileInfo)(movie: Movie): Boolean = {
    
    if (dis.all.year.isDefined && (movie.year == 0 || movie.year != dis.all.year.get)) {
      trace("IGNORE " + movie + " because it specifies different year than " + dis)
      return false
    }
    
    true
  }
}