package moviemetase
package test

object MovieFactory {
  def main(args: Array[String]) {
    val tr = MovieInfos.TitleWithRelease("Inception (2010)")
    val movie = Movie(tr :: Nil)
    println(movie)
  }
}