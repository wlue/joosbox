package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class SymbolSpec extends Specification {
  "Symbol" should {
    "Create an epsilon" in {
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
