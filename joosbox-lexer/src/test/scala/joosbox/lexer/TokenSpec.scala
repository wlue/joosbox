package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class TokenSpec extends Specification {
  "Token" should {
    "match question" in {
      "success" in {
        TokenNFA.nfas(Token.Question).toDFA.consume("?") must beEqualTo(Some(State("?"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Question).toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match parens" in {
      "leftparen success" in {
        TokenNFA.nfas(Token.LeftParen).toDFA.consume("(") must beEqualTo(Some(State("("), ""))
      }
      "leftparen failure" in {
        TokenNFA.nfas(Token.LeftParen).toDFA.consume(")") must beEqualTo(None)
      }
      "rightparen success" in {
        TokenNFA.nfas(Token.RightParen).toDFA.consume(")") must beEqualTo(Some(State(")"), ""))
      }
      "rightparen failure" in {
        TokenNFA.nfas(Token.RightParen).toDFA.consume("(") must beEqualTo(None)
      }
    }

    "match brackets" in {
      "leftbracket success" in {
        TokenNFA.nfas(Token.LeftBracket).toDFA.consume("[") must beEqualTo(Some(State("["), ""))
      }
      "leftbracket failure" in {
        TokenNFA.nfas(Token.LeftBracket).toDFA.consume("]") must beEqualTo(None)
      }
      "rightbracket success" in {
        TokenNFA.nfas(Token.RightBracket).toDFA.consume("]") must beEqualTo(Some(State("]"), ""))
      }
      "rightbracket failure" in {
        TokenNFA.nfas(Token.RightBracket).toDFA.consume("[") must beEqualTo(None)
      }
    }

    "match curlies" in {
      "leftcurly success" in {
        TokenNFA.nfas(Token.LeftCurly).toDFA.consume("{") must beEqualTo(Some(State("{"), ""))
      }
      "leftcurly failure" in {
        TokenNFA.nfas(Token.LeftCurly).toDFA.consume("(") must beEqualTo(None)
      }
      "rightcurly success" in {
        TokenNFA.nfas(Token.RightCurly).toDFA.consume("}") must beEqualTo(Some(State("}"), ""))
      }
      "rightcurly failure" in {
        TokenNFA.nfas(Token.RightCurly).toDFA.consume("]") must beEqualTo(None)
      }
    }

    "match colon" in {
      "success" in {
        TokenNFA.nfas(Token.Colon).toDFA.consume(":") must beEqualTo(Some(State(":"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Colon).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match comma" in {
      "success" in {
        TokenNFA.nfas(Token.Comma).toDFA.consume(",") must beEqualTo(Some(State(","), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Comma).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match dot" in {
      "success" in {
        TokenNFA.nfas(Token.Dot).toDFA.consume(".") must beEqualTo(Some(State("."), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Dot).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match assign" in {
      "success" in {
        TokenNFA.nfas(Token.Assign).toDFA.consume("=") must beEqualTo(Some(State("="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Assign).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match equal" in {
      "success" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume("==") must beEqualTo(Some(State("=="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume("=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match logical not" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalNot).toDFA.consume("!") must beEqualTo(Some(State("!"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalNot).toDFA.consume("~") must beEqualTo(None)
      }
    }

    "match binary not" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryNot).toDFA.consume("~") must beEqualTo(Some(State("~"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryNot).toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match not equal" in {
      "success" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("!=") must beEqualTo(Some(State("!="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("!") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("==") must beEqualTo(None)
      }
    }

    "match divide" in {
      "success" in {
        TokenNFA.nfas(Token.Divide).toDFA.consume("/") must beEqualTo(Some(State("/"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Divide).toDFA.consume("\\") must beEqualTo(None)
      }
    }

    "match divide-assign" in {
      "success" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("/=") must beEqualTo(Some(State("/="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("\\=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("/") must beEqualTo(None)
      }
    }

    "match plus" in {
      "success" in {
        TokenNFA.nfas(Token.Plus).toDFA.consume("+") must beEqualTo(Some(State("+"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Plus).toDFA.consume("-") must beEqualTo(None)
      }
    }

    "match plus-assign" in {
      "success" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("+=") must beEqualTo(Some(State("+="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("+") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match increment" in {
      "success" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("++") must beEqualTo(Some(State("++"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("+") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match minus" in {
      "success" in {
        TokenNFA.nfas(Token.Minus).toDFA.consume("-") must beEqualTo(Some(State("-"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Minus).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match minus-assign" in {
      "success" in {
        TokenNFA.nfas(Token.MinusAssign).toDFA.consume("-=") must beEqualTo(Some(State("-="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("-") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match decrement" in {
      "success" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("--") must beEqualTo(Some(State("--"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("-") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star" in {
      "success" in {
        TokenNFA.nfas(Token.Star).toDFA.consume("*") must beEqualTo(Some(State("*"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Star).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star-assign" in {
      "success" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("*=") must beEqualTo(Some(State("*="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("*") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match modulo" in {
      "success" in {
        TokenNFA.nfas(Token.Modulo).toDFA.consume("%") must beEqualTo(Some(State("%"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Modulo).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match modulo-assign" in {
      "success" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("%=") must beEqualTo(Some(State("%="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("%") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match greater than" in {
      "success" in {
        TokenNFA.nfas(Token.GreaterThan).toDFA.consume(">") must beEqualTo(Some(State(">"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.GreaterThan).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match greater than equal" in {
      "success" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume(">=") must beEqualTo(Some(State(">="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume(">") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift right" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">>") must beEqualTo(Some(State(">>"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match shift right assign" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">>=") must beEqualTo(Some(State(">>="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">=") must beEqualTo(None)
      }
    }

    "match binary shift right" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>>") must beEqualTo(Some(State(">>>"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>") must beEqualTo(None)
      }
    }

    "match binary shift right assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>>=") must beEqualTo(Some(State(">>>="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>>") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
    }
  }
}
