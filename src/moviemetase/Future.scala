package moviemetase

import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReferenceArray
import java.lang.AssertionError
import java.util.concurrent.ForkJoinPool


object Result {
  //  create a result by evaluating the passed expression,
  // returning either a Value or a Failure
  def apply[A](a: =>A): Result[A] =
    try   { Value(a) }
    catch { case e => Failure(e) }
}

sealed trait Result[+A] {
  def isValue: Boolean
  def isFailure: Boolean
  def apply(): A
  
  def flatMap[B](f: A => Result[B]): Result[B]
  def map[B](f: A => B): Result[B]
  
  def get(): A = apply()
  
  def foreach(f: A => Unit) = if (isValue) f( get() )
  
  def toOption: Option[A] = this match {
    case Value(v)   => Some(v)
    case Failure(_) => None
  }

  def toEither: Either[Throwable, A] = this match {
    case Value(v)   => Right(v)
    case Failure(t) => Left(t)
  }
}

case class Value[A](v: A) extends Result[A] {
  def isValue   = true
  def isFailure = false
  def apply(): A = v
  
  def flatMap[B](f: A => Result[B]): Result[B] =
    try   { f(v) }
    catch { case e => Failure(e) }
  
  def map[B](f: A => B): Result[B] =
    try   { Value( f(v) ) }
    catch { case e => Failure(e) } 
}

case class Failure[A](t: Throwable) extends Result[A] {
  def isValue   = false
  def isFailure = true
  def apply(): A = throw t
  
  def flatMap[B](f: A => Result[B]): Result[B] = Failure[B](t)
  def map[B](f: A => B): Result[B] = Failure[B](t)
}

object Future {
  def const[A](b: Result[A]): Future[A] =
    new Future[A]( Some(b) )

  def value[A](v: A): Future[A] =
    const[A]( Value(v) )
    
  def failure[A](t: Throwable): Future[A] =
    const[A]( Failure(t) )
  
  def create[A](f: Future[A] => Unit): Future[A] = {
    val fut = new Future[A]( None )
    f(fut)
    fut
  }

  def join[A](fs: Seq[Future[A]]): Future[Seq[A]] = {
    if (fs.isEmpty)
      return value( Nil )
    
    val size  = fs.size
    val count = new AtomicInteger(size)
    val res   = new AtomicReferenceArray[A](size) {
      def toSeq: Seq[A] = {
        var s: List[A] = Nil
        var i = 0
        while (i < size) {
          s = get(i) :: s
          i = i + 1
        }
        s.reverse
      }
    }

    create[Seq[A]] { newFut =>
      for ( (fut,i) <- fs.zipWithIndex) {
        fut onResult {
          case Value(v) => {
            res.set(i, v)
            if (count.decrementAndGet() == 0)
              newFut set res.toSeq // counting down only on success, nothing failed until here
          }
          case Failure(e) => newFut fail e
        }
      }
    }
  }
}

class Future[A](initialValue: Option[Result[A]] = None) {
  sealed trait State
    case class Waiting(q: List[Result[A] => Unit]) extends State
    case class Completed(b: Result[A]) extends State
  
  private val _state = new AtomicReference[State]( initialValue match {
    case Some(v) => Completed( v )
    case None    => Waiting( Nil )
  })

  @inline
  def state: State = _state.get()

  @inline
  private def cas(cmp: State, set: State): Boolean = _state.compareAndSet(cmp, set)

  // synchronous behaviour
  private val sync = new CountDownLatch(1)
  
  @tailrec
  final def tryBind(b: Result[A]): Boolean = state match {
    case Completed(_) => false
    case s@Waiting(q) => {
      if (cas(s, Completed(b))) {
        callback(b, q)
        sync.countDown() // notify synchronous consumers
        true
      } else tryBind(b) // try again
    }
  }

  def trySet(v: A): Boolean = tryBind( Value(v) )
  
  def tryFail(t: Throwable): Boolean = tryBind( Failure(t) )
  
  def bind(b: Result[A]): Unit =
    if (!tryBind(b))
      throw new IllegalStateException("already bound")
  
  def set(v: A): Unit =
    if (!trySet(v))
      throw new IllegalStateException("already bound") 
  
  // magic =
  def update(v: A): Unit = set(v)
  
  def fail(t: Throwable): Unit =
    if (!tryFail(t))
      throw new IllegalStateException("already bound")

  def isDefined: Boolean = state match {
    case Completed(_) => true
    case _            => false
  }
  
  def isFailed: Boolean = state match {
     case Completed(Failure(_)) => true
     case _                     => false
  }
  
  // register callback to react on the future's result which can be a Value or a Failure
  def onResult(cb: Result[A] => Unit): Future[A] = {
    register(cb)
    this
  }
  
  // register callback to react if and when the future's result is a Value
  def onValue(cb: A => Unit): Future[A] =
    onResult {
      case Value(v) => cb(v)
      case _ =>
    }
  
  // register callback to react if and when the future's result is a Failure
  def onFailure(cb: Throwable => Unit): Future[A] =
    onResult {
      case Failure(t) => cb(t)
      case _ =>
    }
  
  // callback all functions from queue
  @inline
  private def callback(r: Result[A], _q: Seq[Result[A] => Unit]): Unit = {
    var q = _q
    while (!q.isEmpty) {
      val cb = q.head
      Scheduler schedule { cb(r) }
      q = q.tail
    }
  }
  
  // register a new callback-function in the Waiting's queue
  // or call directly if already bound
  @tailrec
  private def register(cb: Result[A] => Unit): Unit = state match {
    case s@Waiting(q) =>
      if (!cas(s, Waiting(cb :: q)))
        register(cb) // try again

    case Completed(res) =>
      callback(res, cb :: Nil)
  }
  
  // synchronously await the future's result
  def await(): A = {
    sync.await()
    state match {
      case Waiting(_)            => throw new AssertionError("Waiting Future has been awaited")
      case Completed(Value(v))   => v
      case Completed(Failure(t)) => throw t
    }
  }
      
  def flatMap[B](f: A => Future[B]): Future[B] =
    Future.create[B] { newFut =>
      onResult {
        case Value(a)   => f(a) onResult { newFut bind _ }
        case Failure(t) => newFut fail t
      }
    }
  
  def map[B](f: A => B): Future[B] =
    Future.create[B] { newFut =>
      onResult {
        case Value(a)   => newFut set f(a)
        case Failure(t) => newFut fail t
      }
    }
  
  def foreach(f: A => Unit): Unit =
    onResult {
      case Value(a)   => f(a)
      case Failure(t) => //throw t //XXX throw?
    }

  def filter(p: A => Boolean): Future[A] =
    Future.create[A] { newFut =>
      onResult {
        case r@Value(v) =>
          if (p(v)) newFut bind r
          //else newFut fail new Future.PredicateNotSatisfied
        
        case Failure(t) => newFut fail t
      }
    }

  override def toString = "Future(%s)@%s".format(state, hashCode)
}

object FutureTest {
  
  val f1 = new Future[Boolean]
  val f2 = new Future[Boolean]
  val f3 = new Future[Boolean]
  
  def main(args: Array[String]): Unit = {
    
    //testJoin
    //testMap
    testForeach
        
    Thread.sleep(3000)
    System.exit(0)
  }
  
  def testForeach {
    for (a <- f1) {
      out("a", a)
    }
    
    f1 set true
    //f1 fail new Exception("fail")
  }
  
  def testMap {
    val x = for (a <- f1; b <- f2) yield (a,b)
    
    x onResult { out("yield", _) }
    
    f1 set true
    //f1 fail new Exception("fail")
    //f2 set false
    f2 fail new Exception("fail")
  }
  
  def testJoin {
    f1 onResult (v => out("f1", v))
    f2 onResult (v => out("f2", v))
    val j = Future.join( f1 :: f2 :: Nil)
    j onResult (b => out("joined", b))
    
    f2 set true
    //f1 set false
    f1 fail new Exception("fail")
  }
  
  def out(lbl: String, x: Any) {
    println(Thread.currentThread().getName() + " " + lbl + ": " + x)
  }
}