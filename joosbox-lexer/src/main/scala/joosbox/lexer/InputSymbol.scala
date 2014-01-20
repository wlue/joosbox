package joosbox.lexer

//  Note that an InputSymbol is either a valid Unicode
//  character or the empty string, to indicate an epsilon-transition.

object Symbol {
  def apply(symbol: String): Symbol = symbol match {
    case "" => Epsilon
    case _ => InputSymbol(symbol)
  }
  def epsilon = Epsilon
}

/**
 * Symbol type.
 */
sealed trait Symbol

/**
 * Regular symbol.
 */
case class InputSymbol(symbol: String) extends Symbol

/**
 * Epsilon symbol.
 */
case object Epsilon extends Symbol {
  def symbol = throw new NoSuchElementException("Epsilon.symbol")
}
