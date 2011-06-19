package moviemetase
import java.util.concurrent._

object WorkerPool {
  val MAX_THREADS = 8
  
  lazy val executor: ExecutorService =
    Executors.newFixedThreadPool( MAX_THREADS )
  
  def shutdown = {
    executor.shutdownNow
    executor.awaitTermination(1, TimeUnit.SECONDS)
    ()
  }
  
  def submit[T](task: Callable[T]): Future[T] = 
    executor submit task
  
  def submit[T](code: => T): Future[T] = 
    submit[T]( new Callable[T] { def call(): T = code } )
}