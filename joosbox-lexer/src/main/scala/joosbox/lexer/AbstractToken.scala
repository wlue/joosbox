package joosbox.lexer

object TokenPriorities {
  val Token = 2
  val KeywordToken = 1
}

abstract class TokenType {
  def apply(data: String = ""): Token
  def priority: Int = TokenPriorities.Token
  def name: String = this.getClass.getSimpleName.replace("$", "")
}

abstract class FixedTokenType extends TokenType {
  def value: String
  def verify(incoming: String) : String = {
    if (incoming != value) {
      throw new IllegalArgumentException("Fixed token type constructor expected \"" + value + "\", got \"" + incoming + "\".")
    }
    incoming
  }
}

abstract class VariableTokenType extends TokenType {
  def verify(incoming: String) : String = incoming
}

class CombinedTokenType(val data: String = "") extends VariableTokenType {
  override def apply(data: String = ""): Token = throw new IllegalArgumentException("Cannot apply() on CombinedToken.")
  override def toString: String = data
  override def equals(obj: Any) = obj match {
    case c: CombinedTokenType => c.data.equals(data)
    case _ => false
  }
}

object CombinedTokenType extends VariableTokenType {
  //  We can't instantiate the CombinedToken - it's really just to make naming of NFA states nicer.
  def createFrom(data: String = ""): TokenType = new CombinedTokenType(data)
  override def apply(data: String = ""): Token = throw new IllegalArgumentException("Cannot apply() on CombinedTokenType.")
}

abstract class KeywordTokenType extends FixedTokenType {
  override def priority: Int = TokenPriorities.KeywordToken
}

abstract class Token(val data: String = "") {
  def priority: Int = TokenPriorities.Token
  def tokenType: TokenType
}

abstract class FixedToken(data: String) extends Token(data)
abstract class VariableToken(data: String) extends Token(data)
abstract class KeywordToken(data: String) extends FixedToken(data) {
  override def priority: Int = TokenPriorities.KeywordToken
}