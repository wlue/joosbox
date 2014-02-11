package joosbox.lexer

object TokenPriorities {
  val Token = 2
  val KeywordToken = 1
}

sealed trait TokenType {
  def create(data: InputString): Token
  def priority: Int = TokenPriorities.Token
  def name: String = this.getClass.getSimpleName.replace("$", "")
  def verify(incoming: InputString): InputString = incoming
}

trait FixedTokenType extends TokenType {
  def value: String
  override def verify(incoming: InputString): InputString = {
    if (incoming.value != value) {
      throw new IllegalArgumentException("Fixed token type constructor expected \"" + value + "\", got \"" + incoming + "\".")
    }
    incoming
  }
}

trait VariableTokenType extends TokenType
class CombinedTokenType(val data: InputString) extends VariableTokenType {
  override def create(data: InputString): Token = throw new IllegalArgumentException("Cannot create() on CombinedTokenType.")
  override def toString: String = data.value
  override def equals(obj: Any) = obj match {
    case c: CombinedTokenType => c.data.equals(data)
    case _ => false
  }
}

/**
 * We can't instantiate the CombinedToken - it's really just to make naming of NFA states nicer.
 */
object CombinedTokenType extends VariableTokenType {
  def createFrom(data: String): TokenType = new CombinedTokenType(InputString(data))
  override def create(data: InputString): Token = throw new IllegalArgumentException("Cannot create() on CombinedTokenType.")
}

trait KeywordTokenType extends FixedTokenType {
  override def priority: Int = TokenPriorities.KeywordToken
}
