package moviemetase
package search

import search._
import java.util.concurrent._
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.util.concurrent.Executors
import java.net.URLConnection
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream


case class MovieSearch(val out: PrintStream = System.out) extends SearchManager[Movie] {
  
  def searchByTerm(term: String): List[Movie] = {
    val search = new GoogleTermAndImdbLink(term) with TmdbIntegrator
    
    val res = search.execute()
        
    sortMovies(res)
  }
  
  def searchByFile(fileInfo: FileInfo): List[Movie] = {
    
    val fut1 = {
      val term = fileInfo.fileName
      val search = new GoogleTermAndImdbLink(term) with TmdbIntegrator
      //s.out = out
      
      search.submit()
    }
    
    val fut2 = {
      val term = fileInfo.dirName
      val search = new GoogleTermAndImdbLink(term) with TmdbIntegrator
      //s.out = out
      
      search.submit()
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