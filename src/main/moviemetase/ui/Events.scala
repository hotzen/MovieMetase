package moviemetase
package ui

import scala.swing.event.Event

case class MovieFileScanned(file: FileInfo) extends Event
case class SearchingMoviesByFile(file: FileInfo) extends Event
case class FoundMoviesByFile(file: FileInfo, movies: List[Movie]) extends Event
case class SearchingMoviesByFileFailed(file: FileInfo, t: Throwable) extends Event

case class SearchSelected(search: Search) extends Event
case class MovieSelected(movie: Movie) extends Event