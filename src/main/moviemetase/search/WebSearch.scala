package moviemetase.search

import scala.concurrent.Future
import java.net.URL

sealed trait WebQuery {
  
}
object WebQuery {
  case class Term(term: String) extends WebQuery
  case class TermLinkingTo(term: String, linkTo: String) extends WebQuery
  case class TermAt(term: String, at: String) extends WebQuery
}


class WebSearch {
  def search(q: WebQuery): Future[List[WebResult]] = {
    null
  }
}

case class WebResult(query: String, url: URL, title: String, snippet: String)