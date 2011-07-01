package moviemetase
package search

import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream

case class MovieSearch(val out: PrintStream = System.out) extends SearchManager[Movie] {
    
  def searchTerm(term: String): List[(Double,Movie)] = {
    val s = new TermWithImdbLinkSearch("FileNameWithImdbLink") with TmdbIntegrator
    s.out = out
    
    val fut = s.execute( term )
    val res = fut.get()
    
    filter(res)
  }
  
  def searchByFile(fileInfo: FileInfo): List[(Double,Movie)] = {
    
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
    
    val res = {
      fut1.get() :: fut2.get() :: Nil
    }
    
    filter(res.flatten)
  }
}