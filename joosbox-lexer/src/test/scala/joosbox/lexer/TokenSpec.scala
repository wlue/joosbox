package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class TokenSpec extends Specification {
  "Token" should {
    "match question" in {
      "success" in {
        TokenNFA.nfas.get(Token.Question).get.toDFA.consume("?") must beEqualTo(Some(State("?"), ""))
      }
      "failure" in {
        TokenNFA.nfas.get(Token.Question).get.toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match parens" in {
      "leftparen success" in {
        TokenNFA.nfas.get(Token.LeftParen).get.toDFA.consume("(") must beEqualTo(Some(State("("), ""))
      }
      "leftparen failure" in {
        TokenNFA.nfas.get(Token.LeftParen).get.toDFA.consume(")") must beEqualTo(None)
      }
      "rightparen success" in {
        TokenNFA.nfas.get(Token.RightParen).get.toDFA.consume(")") must beEqualTo(Some(State(")"), ""))
      }
      "rightparen failure" in {
        TokenNFA.nfas.get(Token.RightParen).get.toDFA.consume("(") must beEqualTo(None)
      }
    }
  }
}
