package joosbox.lexer

object SymbolImplicits {
  import scala.language.implicitConversions
  implicit def stringToSymbol(string: String): Symbol = Symbol(string)
}

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
case object Epsilon extends Symbol

/**
 * Negated symbol
 */
case class NegatedSymbol(symbol: Set[Symbol]) extends Symbol{
  def apply(symbol: Set[Symbol]): NegatedSymbol = symbol match {
      case _ => NegatedSymbol(symbol)
  }
}

/**
 * Digit symbol
 */
case object DigitSymbol extends Symbol {
    Set(Symbol("0"),
        Symbol("1"),
        Symbol("2"),
        Symbol("3"),
        Symbol("4"),
        Symbol("5"),
        Symbol("6"),
        Symbol("7"),
        Symbol("8"),
        Symbol("9"))
}

/**
 * Letter symbol
 */
case object LetterSymbol extends Symbol {
    Set(Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k"),
        Symbol("l"),
        Symbol("m"),
        Symbol("n"),
        Symbol("o"),
        Symbol("p"),
        Symbol("q"),
        Symbol("r"),
        Symbol("s"),
        Symbol("t"),
        Symbol("u"),
        Symbol("v"),
        Symbol("w"),
        Symbol("x"),
        Symbol("y"),
        Symbol("z"))
}




