package joosbox.lexer

object StateImplicits {
  import scala.language.implicitConversions
  implicit def stringToState(string: String): State = State(string)
}

object State {
  def combine(states: Iterable[State]) = State(states.map(_.name).toList.sorted.mkString(","))
  def prefixed(prefix: Token.Kind, state: State) = State(prefix.tokenKind + "-" + state.name)
}

case class State(name: String)
