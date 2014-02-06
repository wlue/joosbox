package joosbox.lexer

object TokenPriorities {
  val Token = 2
  val KeywordToken = 1
}

abstract class TokenType {
  def apply(data: InputString): Token
  def priority: Int = TokenPriorities.Token
  def name: String = this.getClass.getSimpleName.replace("$", "")
  def verify(incoming: InputString) : InputString = incoming
}

abstract class FixedTokenType extends TokenType {
  def value: String
  override def verify(incoming: InputString) : InputString = {
    if (incoming.value != value) {
      throw new IllegalArgumentException("Fixed token type constructor expected \"" + value + "\", got \"" + incoming + "\".")
    }
    incoming
  }
}

abstract class VariableTokenType extends TokenType
class CombinedTokenType(val data: InputString) extends VariableTokenType {
  override def apply(data: InputString): Token = throw new IllegalArgumentException("Cannot apply() on CombinedToken.")
  override def toString: String = data.value
  override def equals(obj: Any) = obj match {
    case c: CombinedTokenType => c.data.equals(data)
    case _ => false
  }
}

object CombinedTokenType extends VariableTokenType {
  //  We can't instantiate the CombinedToken - it's really just to make naming of NFA states nicer.
  def createFrom(data: String): TokenType = new CombinedTokenType(InputString(data))
  override def apply(data: InputString): Token = throw new IllegalArgumentException("Cannot apply() on CombinedTokenType.")
}

abstract class KeywordTokenType extends FixedTokenType {
  override def priority: Int = TokenPriorities.KeywordToken
}

abstract class Token(val data: InputString) {
  def priority: Int = TokenPriorities.Token
  def tokenType: TokenType
}

abstract class FixedToken(data: InputString) extends Token(data)
abstract class VariableToken(data: InputString) extends Token(data)
abstract class KeywordToken(data: InputString) extends FixedToken(data) {
  override def priority: Int = TokenPriorities.KeywordToken
}