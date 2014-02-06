package joosbox.lexer

class SyntaxError(msg: String) extends RuntimeException(msg)
object SyntaxError {
  def apply(token: Token, message: String = ""): SyntaxError = new SyntaxError("Invalid token " + token.data + ". " + message)
}