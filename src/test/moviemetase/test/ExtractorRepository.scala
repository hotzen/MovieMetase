package moviemetase
package test

import extraction._
import org.scalatest.FunSuite

object ExtractorRepositoryTest extends FunSuite {
  import TaskManager.async._
  Logging.level = LogLevel.Trace
  
  test("DefaultRepository") {
    val fut = Repository.load()
    val extractors = fut.await()
    println( extractors.size )
  }
    
  def main(args: Array[String]): Unit = {
    execute(color = false)
    App.shutdown
  }
}