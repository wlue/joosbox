package joosbox.lexer

abstract class Token(val data: InputString) {
  def priority: Int = TokenPriorities.Token
  def tokenType: TokenType
}

abstract class FixedToken(data: InputString) extends Token(data)
abstract class VariableToken(data: InputString) extends Token(data)
abstract class KeywordToken(data: InputString) extends FixedToken(data) {
  override def priority: Int = TokenPriorities.KeywordToken
}
