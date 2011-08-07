package moviemetase
package search

import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream

case class MovieSearch(val out: PrintStream = System.out) extends SearchManager[Movie] {
  
  def searchTerm(term: String): List[Movie] = {
    val s = new TermWithImdbLinkSearch("FileNameWithImdbLink") with TmdbIntegrator
    s.out = out
    
    val fut = s.execute( term )
    val res = fut.get()
    
    sortMovies(res)
  }
  
  def searchByFile(fileInfo: FileInfo): List[Movie] = {
    
    val fut1 = {
      val t = fileInfo.fileName
      val s = new TermWithImdbLinkSearch("FileNameWithImdbLink") with TmdbIntegrator
      s.out = out
      
      s.execute( t )
    }
    
    val fut2 = {
      val t = fileInfo.dirName
      val s = new TermWithImdbLinkSearch("DirNameWithImdbLink") with TmdbIntegrator
      s.out = out
      
      s.execute( t )
    }
    
    val fut3 = {
//      val t = fileInfo.dirName
//      val s = new TermWithImdbLinkSearch("DirNameWithImdbLink") with TmdbIntegrator
//      s.out = out
//      s.execute( t )
    }
        
    val res = {
      fut1.get() :: fut2.get() :: Nil
    }
    
    sortMovies(res.flatten)
  }
        
  def sortMovies(ms: List[Movie]): List[Movie] =
    ms.sortWith( (m1,m2) => {
      val scores1 = m1.infos.collect({ case MovieInfos.Score(score) => score})
      val scores2 = m2.infos.collect({ case MovieInfos.Score(score) => score})
      
      val s1 = if (scores1.isEmpty) 0.3
               else scores1.max
      
      val s2 = if (scores2.isEmpty) 0.3
               else scores2.max
      
      s1 <= s2
    })
}