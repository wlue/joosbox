package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class SymbolSpec extends Specification {
  "Symbol" should {
    "implicity convert from string" in {
      import SymbolImplicits._

      "input symbol" in {
        val symbol: Symbol = "s"
        symbol.isInstanceOf[InputSymbol]
      }

      "epsilon" in {
        val symbol: Symbol = ""
        symbol must beEqualTo(Epsilon)
      }
    }

    "create an epsilon" in {
      Symbol.epsilon must not(throwA[Exception])
      Epsilon must not(throwA[Exception])
    }
  }

  "InputSymbol" should {
    "return the same symbol string" in {
      InputSymbol("a").symbol must beEqualTo("a")
    }
  }
}
