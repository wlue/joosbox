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

    "match less than" in {
      "success" in {
        TokenNFA.nfas(Token.LessThan).toDFA.consume("<") must beEqualTo(Some(State("<"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.LessThan).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match less than equal" in {
      "success" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("<=") must beEqualTo(Some(State("<="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("<") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift left" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<<") must beEqualTo(Some(State("<<"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match shift left assign" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<<=") must beEqualTo(Some(State("<<="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<<") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<=") must beEqualTo(None)
      }
    }

    "match binary xor" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryXor).toDFA.consume("^") must beEqualTo(Some(State("^"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryXor).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match binary xor assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^=") must beEqualTo(Some(State("^="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^ ") must beEqualTo(None)
      }
    }


    "match binary or" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryOr).toDFA.consume("|") must beEqualTo(Some(State("|"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryOr).toDFA.consume("^") must beEqualTo(None)
      }
    }

    "match binary or assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("|=") must beEqualTo(Some(State("|="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("|") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("| ") must beEqualTo(None)
      }
    }

    "match binary and" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryAnd).toDFA.consume("&") must beEqualTo(Some(State("&"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryAnd).toDFA.consume("|") must beEqualTo(None)
      }
    }

    "match binary and assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("&=") must beEqualTo(Some(State("&="), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("&") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("& ") must beEqualTo(None)
      }
    }


    "match logical or" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("||") must beEqualTo(Some(State("||"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("|") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("| ") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("&&") must beEqualTo(None)
      }
    }

    "match logical and" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("&&") must beEqualTo(Some(State("&&"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("&") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("& ") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("||") must beEqualTo(None)
      }
    }

    "match semicolon" in {
      "success" in {
        TokenNFA.nfas(Token.Semicolon).toDFA.consume(";") must beEqualTo(Some(State(";"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Semicolon).toDFA.consume("|") must beEqualTo(None)
      }
    }

    "match whitespace" in {
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume(" ") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\r") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\t") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\f") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\13") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\r\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\n\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\r\r") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\r\r\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("      ") must beEqualTo(Some(State("ws"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\b") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("a  ") must beEqualTo(None)
      }
    }

    "match single line comment" in {
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("//\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello World\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello // World\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("//\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello World\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello // World\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("//\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello World\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("// Hello // World\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("/**\n* Hello\n* World\n */") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("/* Hello World */") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("Hello // World\n") must beEqualTo(None)
      }

    }
  }
}