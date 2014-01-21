package joosbox.lexer

object StateImplicits {
  import scala.language.implicitConversions
  implicit def stringToState(string: String): State = State(string)
}

case class State(name: String)
