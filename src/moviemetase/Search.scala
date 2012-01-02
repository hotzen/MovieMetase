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

// A Query is a task that returns some A, when queried with a String  
//trait Query[A] extends Task[A] {
//  def query: String
//}

// A Search-Manager that chooses the right Strategy
abstract class SearchManager[A] {
  def searchByTerm(term: String): List[A]
  def searchByFile(fileInfo: FileInfo): List[A]
}


//trait SearchStrategy[A] extends Task[A] {
//  def search(term: String): List[A]
//}
