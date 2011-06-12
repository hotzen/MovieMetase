package moviemetase
import java.util.concurrent._

object Scheduler {
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
}