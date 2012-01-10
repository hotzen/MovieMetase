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

trait SearchManager[A] {
  def searchByTerm(term: String): List[A]
  def searchByFile(fileInfo: FileInfo): List[A]
}