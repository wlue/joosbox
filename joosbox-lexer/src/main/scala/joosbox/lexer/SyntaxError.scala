package joosbox.lexer

object SyntaxError {
  def apply(token: Token, message: String = ""): SyntaxError =
    new SyntaxError("Invalid token " + token.data + ". " + message)
}

class SyntaxError(msg: String) extends RuntimeException(msg)
