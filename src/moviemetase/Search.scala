package moviemetase

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

// A Query-Task that queries something and returns A  
abstract class Query[A] extends TaskFactory[A] {
  def query: String
}


// A Search-Manager that chooses the right Strategy
abstract class SearchManager[A] {
  def searchByTerm(term: String): List[A]
  def searchByFile(fileInfo: FileInfo): List[A]
}


// A generic Search-Strategy
abstract class SearchStrategy[A] {
   
  
  // searching a particular term, BLOCKING
  def search(term: String): List[A]

  // executing the search in parallel
  final def execute(term: String): Future[List[A]] = WorkerPool submit { search(term) }
}
