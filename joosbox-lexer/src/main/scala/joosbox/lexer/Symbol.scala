package joosbox.lexer

object SymbolImplicits {
  import scala.language.implicitConversions
  implicit def stringToSymbol(string: String): Symbol = Symbol(string)
}

/**
 * Symbol type.
 */
object Symbol {
  def apply(symbol: String): Symbol = symbol match {
    case "" => Epsilon
    case _ => InputSymbol(symbol)
  }
  def set(symbols: Iterable[String]): Set[Symbol] = symbols.map { sym => Symbol(sym) }.toSet

  lazy val epsilon = Epsilon
  lazy val digits: Set[Symbol] = (1 to 10).map { num => InputSymbol(num.toString) }.toSet
  lazy val digitsGroup = SymbolGroup(digits)
  lazy val letters: Set[Symbol] = ('a' to 'z').map { letter => InputSymbol(letter.toString) }.toSet
  lazy val lettersGroup = SymbolGroup(letters)
}

sealed trait Symbol {
  def matchSymbol(symbol: String): Boolean
}


/**
 * Epsilon symbol. Matches everything.
 */
case object Epsilon extends Symbol {
  def matchSymbol(symbol: String): Boolean = true
}


/**
 * Regular symbol. Matches a single symbol string.
 */
case class InputSymbol(symbol: String) extends Symbol {
  def matchSymbol(symbol: String): Boolean = (this.symbol == symbol)
}

/**
 * Group of symbols.
 */
case class SymbolGroup(symbols: Set[Symbol]) extends Symbol {
  def matchSymbol(symbol: String): Boolean = symbols.exists { sym => sym.matchSymbol(symbol) }
}


/**
 * Negated symbol
 */
case class NegatedSymbols(symbols: String*) extends Symbol {
  /**
   * `matchSymbol` will return true if all of the symbols do not match the argument.
   */
  def matchSymbol(symbol: String): Boolean = symbols.forall { sym =>
    (symbol != sym)
  }
}
