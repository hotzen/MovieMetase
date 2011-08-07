package moviemetase
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

case class Consolidator(id: String) extends Logging {
  val logID = id
  
  type JustInfos = List[MovieInfo]
  
  def consolidate(movies: List[Movie], infos: List[JustInfos]): (List[Movie], List[JustInfos]) = {
    
//    val moviesQ = Queue() ++= movies
//    val infosQ  = Queue() ++= infos
//    
//    val resMovies = new ListBuffer[Movie]
//    val resInfos  = new ListBuffer[JustInfos]
//    
//    while (!moviesQ.isEmpty && !infosQ.isEmpty) {
//      merge(moviesQ.front, infosQ.front) match {
//        case Some(merged) => {
//          moviesQ.dequeue
//          infosQ.dequeue
//          resMovies append merged 
//        }
//        case None => {
//          
//        }
//      }
//    }
    
    (Nil, Nil)
  }
    
  def merge(movie: Movie, infos: List[MovieInfo]): Option[Movie] = {
    None
  }
}