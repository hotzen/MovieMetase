package moviemetase
package ui

import javax.swing.JComponent
import scala.swing.Publisher
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import scala.swing.Component
import scala.swing.event.Event
import scala.swing.event.MouseClicked
import javax.swing.AbstractAction
import java.awt.event.ActionEvent

case class Action(f: ActionEvent => Unit) extends AbstractAction {
  def actionPerformed(evt: ActionEvent): Unit =  f(evt)
}





class MovieSelection {
  import scala.collection.mutable.Map
    
  val selection = Map[Movie, List[MovieInfo]]()
    
  def selected(m: Movie): Option[List[MovieInfo]] =
    selection get m
 
  def selected(m: Movie, i: MovieInfo): Boolean =
    selection get m match {
      case Some(infos) => infos contains i
      case None        => false
    }

  def select(m: Movie): Unit = selection get m match {
    case None => selection put (m, Nil)
    case _    => // already selected
  }
  
  def select(m: Movie, i: MovieInfo): List[MovieInfo] = {
    val infos = selection get m match {
      case Some(infos) => infos
      case None        => Nil
    }
    val newInfos = i :: infos
    selection put (m, newInfos)
    newInfos
  }
  
  def unselect(): Unit =
    selection.clear()
  
  def unselect(m: Movie): Unit =
    selection remove m
  
  def unselect(m: Movie, i: MovieInfo): List[MovieInfo] = {
    val infos = selection get m match {
      case Some(infos) => infos
      case None        => Nil
    }
    val newInfos = infos.filter(_ != i)
    selection put (m, newInfos)
    newInfos
  }
    
  def getMovie(movies: List[Movie], selector: String): Option[Movie] = {
    val parts = selector split "/"
    if (parts.isEmpty)
      return None
      
    val movieID = parts(0)
    movies.find(m => m.id == movieID) match {
      case Some(movie) => Some(movie)
      case None        => { println("MovieSelection: could not find movie " + movieID + " from selector " + selector); None }
    }
  }
  
  def getMovieInfo(movies: List[Movie], selector: String): Option[(Movie,MovieInfo)] = {
    val parts = selector split "/"
    if (parts.tail.isEmpty)
      return None
    
    getMovie(movies, selector) match {
      case Some(movie) => {
        val movieInfoID = parts(1)
        movie.infos.find(i => i.id == movieInfoID) match {
          case Some(info) => Some((movie, info))
          case None       => { println("MovieSelection: could not find info " + movieInfoID + " from selector " + selector); None }
        } 
      }
      case None => None
    }
  }
    
//  def select(movies: List[Movie], selector: String): Unit = {
//    getMovieInfo(movies, selector) match {
//      case Some((movie,info)) => select(movie, info)
//      case None => 
//    }
//    
//    
//    movies.find(m => m.id == movieID) match {
//      case Some(movie) => {
//        
//        // only movieID specified, select movie
//        if (parts.tail.isEmpty) {
//           select(movie)
//
//        } else {
//          
//        }
//      }
//      case None => println("MovieSelection.select("+selector+") could not find movie " + movieID)
//    }
//  }
}


case class JSelected(comp: JComponent) extends Event
case class JUnselected(comp: JComponent) extends Event

trait JSelectable extends Publisher { self: JComponent =>
  private var _selected: Boolean = false
  
  def selected: Boolean = _selected

  def select(): Unit = {
    _selected = true
    //this setBorder UI.SelectionBorder
    this publish JSelected( this )
  }

  def unselect(): Unit = {
    _selected = false
    this setBorder null
    this publish JUnselected( this )
  }
  
  def select(sel: Boolean): Unit =
    if (sel)
      select()
    else
      unselect()

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = select( !_selected )
  })
}

//XXX not used yet
/*
case class Selected(comp: Component) extends Event
case class Unselected(comp: Component) extends Event

trait Selectable extends Publisher { self: Component =>
  var selected: Boolean = false
  
  listenTo(mouse.clicks)
  reactions += {
    case e:MouseClicked => {
      val thiz = Selectable.this
      
      selected = !selected
      
      if (selected) {
        thiz.border = UI.SelectionBorder
        thiz publish Selected( thiz )
      } else {
        thiz.border = null
        thiz publish Unselected( thiz )
      }
    }
  }
}
*/