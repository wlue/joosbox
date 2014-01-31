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

    "match multiline comment" in {
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/* Hello World */") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/* Hello World\n*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/* Hello\n World*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/* Hello // World*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/**\n* Hello\n*  World\n */") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/**/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("/*\n*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("Hello /* World */") must beEqualTo(None)
      }
    }

    "match javadoc comment" in {
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/** Hello World */") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/** Hello World\n*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/** Hello\n World*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/** Hello // World*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/**\n* Hello\n*  World\n */") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/***/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/**\n*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/*\n**/") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("Hello /** World */") must beEqualTo(None)
      }
    }

    "match number" in {
      "success" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("0") must beEqualTo(Some(State("digit"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("1") must beEqualTo(Some(State("digit"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("10") must beEqualTo(Some(State("digit"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("1234567890") must beEqualTo(Some(State("digit"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("-1") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("one") must beEqualTo(None)
      }
    }

    "match identifier" in {
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("a") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("abc") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("AbCdEf") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("abc_def$") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("$hello") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("_hello") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("world1") must beEqualTo(Some(State("id"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("-world") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("1world") must beEqualTo(None)
      }
    }

    "match char literal" in {
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'a'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'0'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'^'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("' '") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\"'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\n'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\b'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\\''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\0'") must beEqualTo(Some(State("char"), ""))
      }
       "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\7'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\77'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\377'") must beEqualTo(Some(State("char"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'abc'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\n'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\r\\n'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\9'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\97'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\79'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\477'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\397'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\379'") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\3771'") must beEqualTo(None)
      }
    }

    "match string literal" in {
      "success" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"a\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"abc\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"Hello, world!\\n\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"\\\"Nested\\\"\"") must beEqualTo(Some(State("string"), ""))
      }
      "failure" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"\n\"") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"") must beEqualTo(None)
      }
      "failure" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("") must beEqualTo(None)
      }
    }

  }

  "matchString" in {
    "simple assign/equals matching" in {
      val testNFAs = Set[NFA](TokenNFA.nfas(Token.Assign), TokenNFA.nfas(Token.Equal))
      val mergedTestDFA = NFA.union(testNFAs).toDFA

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(Token.Assign("="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(Token.Equal("=="))))
      mergedTestDFA.matchString("===") must beEqualTo(Some(List(Token.Equal("=="), Token.Assign("="))))
      mergedTestDFA.matchString("+") must beEqualTo(None)
    }

    "simple assign/equal/not matching" in {
      val testNFAs = Set[NFA](
        TokenNFA.nfas(Token.Assign),
        TokenNFA.nfas(Token.Equal),
        TokenNFA.nfas(Token.LogicalNot)
      )
      val mergedTestDFA = NFA.union(testNFAs).toDFA

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(Token.Assign("="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(Token.Equal("=="))))
      mergedTestDFA.matchString("!") must beEqualTo(Some(List(Token.LogicalNot("!"))))
    }

    "simple assign/equal/not/notequal matching" in {
      val testNFAs = Set[NFA](
        TokenNFA.nfas(Token.Assign),
        TokenNFA.nfas(Token.Equal),
        TokenNFA.nfas(Token.LogicalNot),
        TokenNFA.nfas(Token.NotEqual)        
      )
      val mergedTestDFA = NFA.union(testNFAs).toDFA

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(Token.Assign("="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(Token.Equal("=="))))
      mergedTestDFA.matchString("!") must beEqualTo(Some(List(Token.LogicalNot("!"))))
      mergedTestDFA.matchString("!=") must beEqualTo(Some(List(Token.NotEqual("!="))))
    }

    "keywords in identifier" in {
      val matchAgainst = TokenNFA.nfas(Token.Identifier).toDFA

      "abstract" in {
        matchAgainst.matchString("abstract") must beEqualTo(Some(List(Token.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        matchAgainst.matchString("boolean") must beEqualTo(Some(List(Token.BooleanKeyword("boolean"))))
      }

      "break" in {
        matchAgainst.matchString("break") must beEqualTo(Some(List(Token.BreakKeyword("break"))))
      }

      "byte" in {
        matchAgainst.matchString("byte") must beEqualTo(Some(List(Token.ByteKeyword("byte"))))
      }

      "case" in {
        matchAgainst.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
      }

      "catch" in {
        matchAgainst.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
      }

      "char" in {
        matchAgainst.matchString("char") must beEqualTo(Some(List(Token.CharKeyword("char"))))
      }

      "class" in {
        matchAgainst.matchString("class") must beEqualTo(Some(List(Token.ClassKeyword("class"))))
      }

      "const" in {
        matchAgainst.matchString("const") must beEqualTo(Some(List(Token.ConstKeyword("const"))))
      }

      "continue" in {
        matchAgainst.matchString("continue") must beEqualTo(Some(List(Token.ContinueKeyword("continue"))))
      }

      "default" in {
        matchAgainst.matchString("default") must beEqualTo(Some(List(Token.DefaultKeyword("default"))))
      }

      "do" in {
        matchAgainst.matchString("do") must beEqualTo(Some(List(Token.DoKeyword("do"))))
      }

      "double" in {
        matchAgainst.matchString("double") must beEqualTo(Some(List(Token.DoubleKeyword("double"))))
      }

      "else" in {
        matchAgainst.matchString("else") must beEqualTo(Some(List(Token.ElseKeyword("else"))))
      }

      "extends" in {
        matchAgainst.matchString("extends") must beEqualTo(Some(List(Token.ExtendsKeyword("extends"))))
      }

      "final" in {
        matchAgainst.matchString("final") must beEqualTo(Some(List(Token.FinalKeyword("final"))))
      }

      "finally" in {
        matchAgainst.matchString("finally") must beEqualTo(Some(List(Token.FinallyKeyword("finally"))))
      }

      "float" in {
        matchAgainst.matchString("float") must beEqualTo(Some(List(Token.FloatKeyword("float"))))
      }

      "for" in {
        matchAgainst.matchString("for") must beEqualTo(Some(List(Token.ForKeyword("for"))))
      }

      "goto" in {
        matchAgainst.matchString("goto") must beEqualTo(Some(List(Token.GotoKeyword("goto"))))
      }

      "if" in {
        matchAgainst.matchString("if") must beEqualTo(Some(List(Token.IfKeyword("if"))))
      }

      "implements" in {
        matchAgainst.matchString("implements") must beEqualTo(Some(List(Token.ImplementsKeyword("implements"))))
      }

      "import" in {
        matchAgainst.matchString("import") must beEqualTo(Some(List(Token.ImportKeyword("import"))))
      }

      "instanceof" in {
        matchAgainst.matchString("instanceof") must beEqualTo(Some(List(Token.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        matchAgainst.matchString("int") must beEqualTo(Some(List(Token.IntKeyword("int"))))
      }

      "interface" in {
        matchAgainst.matchString("interface") must beEqualTo(Some(List(Token.InterfaceKeyword("interface"))))
      }

      "long" in {
        matchAgainst.matchString("long") must beEqualTo(Some(List(Token.LongKeyword("long"))))
      }

      "native" in {
        matchAgainst.matchString("native") must beEqualTo(Some(List(Token.NativeKeyword("native"))))
      }

      "new" in {
        matchAgainst.matchString("new") must beEqualTo(Some(List(Token.NewKeyword("new"))))
      }

      "package" in {
        matchAgainst.matchString("package") must beEqualTo(Some(List(Token.PackageKeyword("package"))))
      }

      "private" in {
        matchAgainst.matchString("private") must beEqualTo(Some(List(Token.PrivateKeyword("private"))))
      }

      "protected" in {
        matchAgainst.matchString("protected") must beEqualTo(Some(List(Token.ProtectedKeyword("protected"))))
      }

      "public" in {
        matchAgainst.matchString("public") must beEqualTo(Some(List(Token.PublicKeyword("public"))))
      }

      "return" in {
        matchAgainst.matchString("return") must beEqualTo(Some(List(Token.ReturnKeyword("return"))))
      }

      "short" in {
        matchAgainst.matchString("short") must beEqualTo(Some(List(Token.ShortKeyword("short"))))
      }

      "static" in {
        matchAgainst.matchString("static") must beEqualTo(Some(List(Token.StaticKeyword("static"))))
      }

      "strictfp" in {
        matchAgainst.matchString("strictfp") must beEqualTo(Some(List(Token.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        matchAgainst.matchString("super") must beEqualTo(Some(List(Token.SuperKeyword("super"))))
      }

      "switch" in {
        matchAgainst.matchString("switch") must beEqualTo(Some(List(Token.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        matchAgainst.matchString("synchronized") must beEqualTo(Some(List(Token.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        matchAgainst.matchString("this") must beEqualTo(Some(List(Token.ThisKeyword("this"))))
      }

      "throw" in {
        matchAgainst.matchString("throw") must beEqualTo(Some(List(Token.ThrowKeyword("throw"))))
      }

      "throws" in {
        matchAgainst.matchString("throws") must beEqualTo(Some(List(Token.ThrowsKeyword("throws"))))
      }

      "transient" in {
        val nfa = TokenNFA.nfas(Token.Identifier)
        matchAgainst.matchString("transient") must beEqualTo(Some(List(Token.TransientKeyword("transient"))))
      }

      "try" in {
        matchAgainst.matchString("try") must beEqualTo(Some(List(Token.TryKeyword("try"))))
      }

      "void" in {
        matchAgainst.matchString("void") must beEqualTo(Some(List(Token.VoidKeyword("void"))))
      }

      "volatile" in {
        matchAgainst.matchString("volatile") must beEqualTo(Some(List(Token.VolatileKeyword("volatile"))))
      }

      "while" in {
        matchAgainst.matchString("while") must beEqualTo(Some(List(Token.WhileKeyword("while"))))
      }


      "true" in {
        matchAgainst.matchString("true") must beEqualTo(Some(List(Token.TrueLiteral("true"))))
      }

      "false" in {
        matchAgainst.matchString("false") must beEqualTo(Some(List(Token.FalseLiteral("false"))))
      }

      "null" in {
        matchAgainst.matchString("null") must beEqualTo(Some(List(Token.NullLiteral("null"))))
      }
    }

    "keywords in total NFA" in {
      val totalDFA: DFA = TokenNFA.nfa.toDFA

      "abstract" in {
        totalDFA.matchString("abstract") must beEqualTo(Some(List(Token.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        totalDFA.matchString("boolean") must beEqualTo(Some(List(Token.BooleanKeyword("boolean"))))
      }

      "break" in {
        totalDFA.matchString("break") must beEqualTo(Some(List(Token.BreakKeyword("break"))))
      }

      "byte" in {
        totalDFA.matchString("byte") must beEqualTo(Some(List(Token.ByteKeyword("byte"))))
      }

      "case" in {
        totalDFA.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
      }

      "catch" in {
        totalDFA.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
      }

      "char" in {
        totalDFA.matchString("char") must beEqualTo(Some(List(Token.CharKeyword("char"))))
      }

      "class" in {
        totalDFA.matchString("class") must beEqualTo(Some(List(Token.ClassKeyword("class"))))
      }

      "const" in {
        totalDFA.matchString("const") must beEqualTo(Some(List(Token.ConstKeyword("const"))))
      }

      "continue" in {
        totalDFA.matchString("continue") must beEqualTo(Some(List(Token.ContinueKeyword("continue"))))
      }

      "default" in {
        totalDFA.matchString("default") must beEqualTo(Some(List(Token.DefaultKeyword("default"))))
      }

      "do" in {
        totalDFA.matchString("do") must beEqualTo(Some(List(Token.DoKeyword("do"))))
      }

      "double" in {
        totalDFA.matchString("double") must beEqualTo(Some(List(Token.DoubleKeyword("double"))))
      }

      "else" in {
        totalDFA.matchString("else") must beEqualTo(Some(List(Token.ElseKeyword("else"))))
      }

      "extends" in {
        totalDFA.matchString("extends") must beEqualTo(Some(List(Token.ExtendsKeyword("extends"))))
      }

      "final" in {
        totalDFA.matchString("final") must beEqualTo(Some(List(Token.FinalKeyword("final"))))
      }

      "finally" in {
        totalDFA.matchString("finally") must beEqualTo(Some(List(Token.FinallyKeyword("finally"))))
      }

      "float" in {
        totalDFA.matchString("float") must beEqualTo(Some(List(Token.FloatKeyword("float"))))
      }

      "for" in {
        totalDFA.matchString("for") must beEqualTo(Some(List(Token.ForKeyword("for"))))
      }

      "goto" in {
        totalDFA.matchString("goto") must beEqualTo(Some(List(Token.GotoKeyword("goto"))))
      }

      "if" in {
        totalDFA.matchString("if") must beEqualTo(Some(List(Token.IfKeyword("if"))))
      }

      "implements" in {
        totalDFA.matchString("implements") must beEqualTo(Some(List(Token.ImplementsKeyword("implements"))))
      }

      "import" in {
        totalDFA.matchString("import") must beEqualTo(Some(List(Token.ImportKeyword("import"))))
      }

      "instanceof" in {
        totalDFA.matchString("instanceof") must beEqualTo(Some(List(Token.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        totalDFA.matchString("int") must beEqualTo(Some(List(Token.IntKeyword("int"))))
      }

      "interface" in {
        totalDFA.matchString("interface") must beEqualTo(Some(List(Token.InterfaceKeyword("interface"))))
      }

      "long" in {
        totalDFA.matchString("long") must beEqualTo(Some(List(Token.LongKeyword("long"))))
      }

      "native" in {
        totalDFA.matchString("native") must beEqualTo(Some(List(Token.NativeKeyword("native"))))
      }

      "new" in {
        totalDFA.matchString("new") must beEqualTo(Some(List(Token.NewKeyword("new"))))
      }

      "package" in {
        totalDFA.matchString("package") must beEqualTo(Some(List(Token.PackageKeyword("package"))))
      }

      "private" in {
        totalDFA.matchString("private") must beEqualTo(Some(List(Token.PrivateKeyword("private"))))
      }

      "protected" in {
        totalDFA.matchString("protected") must beEqualTo(Some(List(Token.ProtectedKeyword("protected"))))
      }

      "public" in {
        totalDFA.matchString("public") must beEqualTo(Some(List(Token.PublicKeyword("public"))))
      }

      "return" in {
        totalDFA.matchString("return") must beEqualTo(Some(List(Token.ReturnKeyword("return"))))
      }

      "short" in {
        totalDFA.matchString("short") must beEqualTo(Some(List(Token.ShortKeyword("short"))))
      }

      "static" in {
        totalDFA.matchString("static") must beEqualTo(Some(List(Token.StaticKeyword("static"))))
      }

      "strictfp" in {
        totalDFA.matchString("strictfp") must beEqualTo(Some(List(Token.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        totalDFA.matchString("super") must beEqualTo(Some(List(Token.SuperKeyword("super"))))
      }

      "switch" in {
        totalDFA.matchString("switch") must beEqualTo(Some(List(Token.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        totalDFA.matchString("synchronized") must beEqualTo(Some(List(Token.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        totalDFA.matchString("this") must beEqualTo(Some(List(Token.ThisKeyword("this"))))
      }

      "throw" in {
        totalDFA.matchString("throw") must beEqualTo(Some(List(Token.ThrowKeyword("throw"))))
      }

      "throws" in {
        totalDFA.matchString("throws") must beEqualTo(Some(List(Token.ThrowsKeyword("throws"))))
      }

      "transient" in {
        totalDFA.matchString("transient") must beEqualTo(Some(List(Token.TransientKeyword("transient"))))
      }

      "try" in {
        totalDFA.matchString("try") must beEqualTo(Some(List(Token.TryKeyword("try"))))
      }

      "void" in {
        totalDFA.matchString("void") must beEqualTo(Some(List(Token.VoidKeyword("void"))))
      }

      "volatile" in {
        totalDFA.matchString("volatile") must beEqualTo(Some(List(Token.VolatileKeyword("volatile"))))
      }

      "while" in {
        totalDFA.matchString("while") must beEqualTo(Some(List(Token.WhileKeyword("while"))))
      }

      "true" in {
        totalDFA.matchString("true") must beEqualTo(Some(List(Token.TrueLiteral("true"))))
      }

      "false" in {
        totalDFA.matchString("false") must beEqualTo(Some(List(Token.FalseLiteral("false"))))
      }

      "null" in {
        totalDFA.matchString("null") must beEqualTo(Some(List(Token.NullLiteral("null"))))
      }
    }

    "similar prefixes" in {
      "case, catch" in {
        val totalDFA: DFA = TokenNFA.nfas(Token.Identifier).toDFA

        totalDFA.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
        totalDFA.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
        totalDFA.matchString("neither") must beEqualTo(Some(List(Token.Identifier("neither"))))
      }
    }

    "matching the entire grammar all at once" in {
      val theDFA = TokenNFA.nfa.toDFA
      theDFA must haveClass[DFA]

      "assign" in {
        theDFA.matchString("=") must beEqualTo(Some(List(Token.Assign("="))))
      }

      "equals" in {
        theDFA.matchString("==") must beEqualTo(Some(List(Token.Equal("=="))))
      }

      "logical not" in {
        theDFA.matchString("!") must beEqualTo(Some(List(Token.LogicalNot("!"))))
      }

      "not equal" in {
        theDFA.matchString("!=") must beEqualTo(Some(List(Token.NotEqual("!="))))
      }

      "variable inequality" in {
        theDFA.matchString("a != b") must beEqualTo(Some(List(
          Token.Identifier("a"),
          Token.Whitespace(" "),
          Token.NotEqual("!="),
          Token.Whitespace(" "),
          Token.Identifier("b")
        )))
      }

      "variable equality and assignment" in {
        theDFA.matchString("boolean areEqual = (a == b);") must beEqualTo(Some(List(
          Token.BooleanKeyword("boolean"),
          Token.Whitespace(" "),
          Token.Identifier("areEqual"),
          Token.Whitespace(" "),
          Token.Assign("="),
          Token.Whitespace(" "),
          Token.LeftParen("("),
          Token.Identifier("a"),
          Token.Whitespace(" "),
          Token.Equal("=="),
          Token.Whitespace(" "),
          Token.Identifier("b"),
          Token.RightParen(")"),
          Token.Semicolon(";")
        )))
      }

      "class declaration" in {
        theDFA.matchString("public static Bicycle(int startGear) { gear = startGear; }") must beEqualTo(Some(List(
          Token.PublicKeyword("public"),
          Token.Whitespace(" "),
          Token.StaticKeyword("static"),
          Token.Whitespace(" "),
          Token.Identifier("Bicycle"),
          Token.LeftParen("("),
          Token.IntKeyword("int"),
          Token.Whitespace(" "),
          Token.Identifier("startGear"),
          Token.RightParen(")"),
          Token.Whitespace(" "),
          Token.LeftCurly("{"),
          Token.Whitespace(" "),
          Token.Identifier("gear"),
          Token.Whitespace(" "),
          Token.Assign("="),
          Token.Whitespace(" "),
          Token.Identifier("startGear"),
          Token.Semicolon(";"),
          Token.Whitespace(" "),
          Token.RightCurly("}")
        )))
      }
    }
  }
}
