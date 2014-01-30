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

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(MatchData("ASSIGN", "="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(MatchData("EQUAL", "=="))))
      mergedTestDFA.matchString("===") must beEqualTo(Some(List(MatchData("EQUAL", "=="), MatchData("ASSIGN", "="))))
      mergedTestDFA.matchString("+") must beEqualTo(None)
    }

    "simple assign/equal/not matching" in {
      val testNFAs = Set[NFA](
        TokenNFA.nfas(Token.Assign),
        TokenNFA.nfas(Token.Equal),
        TokenNFA.nfas(Token.LogicalNot)
      )
      val mergedTestDFA = NFA.union(testNFAs).toDFA

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(MatchData("ASSIGN", "="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(MatchData("EQUAL", "=="))))
      mergedTestDFA.matchString("!") must beEqualTo(Some(List(MatchData("LNOT", "!"))))
    }

    "simple assign/equal/not/notequal matching" in {
      val testNFAs = Set[NFA](
        TokenNFA.nfas(Token.Assign),
        TokenNFA.nfas(Token.Equal),
        TokenNFA.nfas(Token.LogicalNot),
        TokenNFA.nfas(Token.NotEqual)        
      )
      val mergedTestDFA = NFA.union(testNFAs).toDFA

      mergedTestDFA.matchString("=") must beEqualTo(Some(List(MatchData("ASSIGN", "="))))
      mergedTestDFA.matchString("==") must beEqualTo(Some(List(MatchData("EQUAL", "=="))))
      mergedTestDFA.matchString("!") must beEqualTo(Some(List(MatchData("LNOT", "!"))))
      mergedTestDFA.matchString("!=") must beEqualTo(Some(List(MatchData("NOT_EQUAL", "!="))))
    }

    "matching the entire grammar all at once" in {
      val theDFA = TokenNFA.nfa.toDFA
      theDFA must haveClass[DFA]

      "assign" in {
        theDFA.matchString("=") must beEqualTo(Some(List(MatchData("ASSIGN", "="))))
      }

      "equals" in {
        theDFA.matchString("==") must beEqualTo(Some(List(MatchData("EQUAL", "=="))))
      }

      "logical not" in {
        theDFA.matchString("!") must beEqualTo(Some(List(MatchData("LNOT", "!"))))
      }

      "not equal" in {
        theDFA.matchString("!=") must beEqualTo(Some(List(MatchData("NOT_EQUAL", "!="))))
      }

      "variable inequality" in {
        theDFA.matchString("a != b") must beEqualTo(Some(List(
          MatchData("IDENTIFIER", "a"),
          MatchData("WHITESPACE", " "),
          MatchData("NOT_EQUAL", "!="),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "b")
        )))
      }

      "variable equality and assignment" in {
        theDFA.matchString("bool areEqual = (a == b);") must beEqualTo(Some(List(
          MatchData("IDENTIFIER", "bool"),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "areEqual"),
          MatchData("WHITESPACE", " "),
          MatchData("ASSIGN", "="),
          MatchData("WHITESPACE", " "),
          MatchData("LPAREN", "("),
          MatchData("IDENTIFIER", "a"),
          MatchData("WHITESPACE", " "),
          MatchData("EQUAL", "=="),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "b"),
          MatchData("RPAREN", ")"),
          MatchData("SEMI", ";")
        )))
      }

      "class declaration" in {
        theDFA.matchString("public static Bicycle(int startGear) { gear = startGear; }") must beEqualTo(Some(List(
          MatchData("IDENTIFIER", "public"),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "static"),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "Bicycle"),
          MatchData("LPAREN", "("),
          MatchData("IDENTIFIER", "int"),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "startGear"),
          MatchData("RPAREN", ")"),
          MatchData("WHITESPACE", " "),
          MatchData("LCURLY", "{"),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "gear"),
          MatchData("WHITESPACE", " "),
          MatchData("ASSIGN", "="),
          MatchData("WHITESPACE", " "),
          MatchData("IDENTIFIER", "startGear"),
          MatchData("SEMI", ";"),
          MatchData("WHITESPACE", " "),
          MatchData("RCURLY", "}")
        )))
      }
    }
  }
}
