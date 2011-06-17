package moviemetase
package test

object TmdbTest {
  def main(args: Array[String]) = {
    
    val imdbID = "tt1375666"
    
    val q = TMDB.ImdbLookup(imdbID)
    val s = QueryExecutor(q)
    println ( s.call().mkString("\n") )
    
  }
}