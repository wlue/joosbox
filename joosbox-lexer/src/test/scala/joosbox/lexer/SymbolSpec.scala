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
  }

  "Epsilon" should {
    "construct" in {
      Symbol.epsilon must not(throwA[Exception])
      Epsilon must not(throwA[Exception])
    }

    "match everything" in {
      Epsilon.matchSymbol("a") must beEqualTo(true)
      Epsilon.matchSymbol("b") must beEqualTo(true)
      Epsilon.matchSymbol("c") must beEqualTo(true)
    }
  }

  "InputSymbol" should {
    "return the same symbol string" in {
      InputSymbol("a").symbol must beEqualTo("a")
    }

    "match" in {
      InputSymbol("a").matchSymbol("a") must beEqualTo(true)
      InputSymbol("123").matchSymbol("123") must beEqualTo(true)
      InputSymbol("\n").matchSymbol("\n") must beEqualTo(true)
    }
  }

  "SymbolGroup" should {
    "construct" in {
      SymbolGroup(Set(Symbol("a"), Symbol("b"))) must not(throwA[Exception])
    }

    "match" in {
      val sym = SymbolGroup(Set(Symbol("a"), Symbol("b")))
      sym.matchSymbol("a") must beEqualTo(true)
      sym.matchSymbol("b") must beEqualTo(true)
      sym.matchSymbol("c") must beEqualTo(false)
    }
  }

  "NegatedSymbols" should {
    "construct" in {
      NegatedSymbols("a", "b") must not(throwA[Exception])
    }

    "match" in {
      val sym = NegatedSymbols("a", "b")
      sym.matchSymbol("a") must beEqualTo(false)
      sym.matchSymbol("b") must beEqualTo(false)
      sym.matchSymbol("c") must beEqualTo(true)
    }
  }

}
