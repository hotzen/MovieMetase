package moviemetase
package search

import query._
import java.net.URL
//import java.io.InputStream
//import java.io.OutputStream
//import java.io.PrintStream
//import java.util.concurrent.Callable
//import java.util.concurrent.Future
//import java.util.concurrent.Executors
//import java.net.URLConnection
import scala.collection.mutable.ListBuffer
import Util._
import java.io.PrintStream


case class MovieSearch(val out: PrintStream = System.out) extends SearchManager[Movie] with Logging {
  val logID = "MovieSearch"
  
  Logging.out = out
  
  def searchByTerm(term: String): List[Movie] = {
    val movies = new ListBuffer[Movie]
    
    val dis = Dissected(term)
    trace(dis.toString)
    
//    // search *exactly* the term
//    if (movies.isEmpty && !dis.tags.isEmpty) {
//      trace("Exact-Term search...")
//      
//      val t = "\"" + term + "\""
//      movies appendAll GoogleTermWithImdbLink(t, Some(term)).execute()
//    }
//    
//    // search *exactly* name + year + tags
//    if (movies.isEmpty && !dis.names.isEmpty && !dis.tags.isEmpty) {
//      trace("Exact-Name & Year & Tags search ...")
//      
//      val t1 = "\"" + dis.name + "\""
//      val t2 = dis.year match {
//        case Some(year) => t1 + " " + year
//        case None       => t1
//      }
//      val t3 = t2 + " " + dis.tags.mkString(" ")
//      movies appendAll GoogleTermWithImdbLink(t3).execute()
//    }
//    
//    // search *exactly* name + year
//    if (movies.isEmpty && !dis.names.isEmpty) {
//      trace("Exact-Name & Year search ...")
//      
//      val t1 = "\"" + dis.name + "\""
//      val t2 = dis.year match {
//        case Some(year) => t1 + " " + year
//        case None       => t1
//      }
//      movies appendAll GoogleTermAtImdbSite( t2 ).execute()
//    }
    
    // search name + year
    if (movies.isEmpty && !dis.names.isEmpty) {
      trace("Name & Year search ...")
      
      val name = dis.name
      val t = dis.year match {
        case Some(year) => name + " " + year
        case None       => name
      }
      movies appendAll GoogleTermAtImdbSite( t ).execute()
    }
    
    // search everything
//    if (movies.isEmpty) {
//      trace("last resort, full search ...")
//      
//      val t = dis.parts.mkString(" ")
//      movies appendAll GoogleTermAtImdbSite( t ).execute()
//    }
    
    
    // NOTHING FOUND
    if (movies.isEmpty)
      return Nil
    
    
    // auto-complete with TMDB
    movies.map( TMDB.AutoExpandMovie(_).execute() ).toList
  }
  
  def searchByFile(fileInfo: FileInfo): List[Movie] = {
    
    val fut1 = {
      val term = fileInfo.fileName
      val search = new GoogleTermWithImdbLink(term)
      //s.out = out
      
      search.submit()
    }
    
    val fut2 = {
      val term = fileInfo.dirName
      val search = new GoogleTermWithImdbLink(term)
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