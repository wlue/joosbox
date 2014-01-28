package joosbox.lexer

object StateImplicits {
  import scala.language.implicitConversions
  implicit def stringToState(string: String): State = State(string)
}

object State {
  def combine(states: Iterable[State]) = State(states.map(_.name).toList.sorted.mkString(","))
}

case class State(name: String, matchData: Option[MatchData] = None) {
  //  Don't consider matchData when checking for equality.
  //  matchData should only be queried once some Automaton
  //  has returned a State as a match.
  override def equals(obj: Any) = obj match {
    case State(name) => this.name == name
    case _ => false
  }

  override def hashCode: Int = name.hashCode
}
