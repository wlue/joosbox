package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class TokenSpec extends Specification {
  "Token" should {
    "match question" in {
      "success" in {
        TokenNFA.nfas(Token.Question).toDFA.consume("?") must beEqualTo(Some(State("?"), ""))
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.Colon).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match comma" in {
      "success" in {
        TokenNFA.nfas(Token.Comma).toDFA.consume(",") must beEqualTo(Some(State(","), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Comma).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match dot" in {
      "success" in {
        TokenNFA.nfas(Token.Dot).toDFA.consume(".") must beEqualTo(Some(State("."), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Dot).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match assign" in {
      "success" in {
        TokenNFA.nfas(Token.Assign).toDFA.consume("=") must beEqualTo(Some(State("="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Assign).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match equal" in {
      "success" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume("==") must beEqualTo(Some(State("=="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Equal).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match logical not" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalNot).toDFA.consume("!") must beEqualTo(Some(State("!"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalNot).toDFA.consume("~") must beEqualTo(None)
      }
    }

    "match binary not" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryNot).toDFA.consume("~") must beEqualTo(Some(State("~"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryNot).toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match not equal" in {
      "success" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("!=") must beEqualTo(Some(State("!="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("!") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.NotEqual).toDFA.consume("==") must beEqualTo(None)
      }
    }

    "match divide" in {
      "success" in {
        TokenNFA.nfas(Token.Divide).toDFA.consume("/") must beEqualTo(Some(State("/"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Divide).toDFA.consume("\\") must beEqualTo(None)
      }
    }

    "match divide-assign" in {
      "success" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("/=") must beEqualTo(Some(State("/="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("\\=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.DivideAssign).toDFA.consume("/") must beEqualTo(None)
      }
    }

    "match plus" in {
      "success" in {
        TokenNFA.nfas(Token.Plus).toDFA.consume("+") must beEqualTo(Some(State("+"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Plus).toDFA.consume("-") must beEqualTo(None)
      }
    }

    "match plus-assign" in {
      "success" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("+=") must beEqualTo(Some(State("+="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("+") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match increment" in {
      "success" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("++") must beEqualTo(Some(State("++"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("+") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Increment).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match minus" in {
      "success" in {
        TokenNFA.nfas(Token.Minus).toDFA.consume("-") must beEqualTo(Some(State("-"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Minus).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match minus-assign" in {
      "success" in {
        TokenNFA.nfas(Token.MinusAssign).toDFA.consume("-=") must beEqualTo(Some(State("-="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("-") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match decrement" in {
      "success" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("--") must beEqualTo(Some(State("--"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("-") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Decrement).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star" in {
      "success" in {
        TokenNFA.nfas(Token.Star).toDFA.consume("*") must beEqualTo(Some(State("*"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Star).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star-assign" in {
      "success" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("*=") must beEqualTo(Some(State("*="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("*") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.StarAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match modulo" in {
      "success" in {
        TokenNFA.nfas(Token.Modulo).toDFA.consume("%") must beEqualTo(Some(State("%"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Modulo).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match modulo-assign" in {
      "success" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("%=") must beEqualTo(Some(State("%="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("%") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ModuloAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match greater than" in {
      "success" in {
        TokenNFA.nfas(Token.GreaterThan).toDFA.consume(">") must beEqualTo(Some(State(">"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.GreaterThan).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match greater than equal" in {
      "success" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume(">=") must beEqualTo(Some(State(">="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume(">") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.GreaterEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift right" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">>") must beEqualTo(Some(State(">>"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftRight).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match shift right assign" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">>=") must beEqualTo(Some(State(">>="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftRightAssign).toDFA.consume(">=") must beEqualTo(None)
      }
    }

    "match binary shift right" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>>") must beEqualTo(Some(State(">>>"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryShiftRight).toDFA.consume(">>") must beEqualTo(None)
      }
    }

    "match binary shift right assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>>=") must beEqualTo(Some(State(">>>="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>>") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
    }

    "match less than" in {
      "success" in {
        TokenNFA.nfas(Token.LessThan).toDFA.consume("<") must beEqualTo(Some(State("<"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LessThan).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match less than equal" in {
      "success" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("<=") must beEqualTo(Some(State("<="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("<") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LessEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift left" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<<") must beEqualTo(Some(State("<<"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftLeft).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match shift left assign" in {
      "success" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<<=") must beEqualTo(Some(State("<<="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<<") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.ShiftLeftAssign).toDFA.consume("<=") must beEqualTo(None)
      }
    }

    "match binary xor" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryXor).toDFA.consume("^") must beEqualTo(Some(State("^"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryXor).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match binary xor assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^=") must beEqualTo(Some(State("^="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryXorAssign).toDFA.consume("^ ") must beEqualTo(None)
      }
    }


    "match binary or" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryOr).toDFA.consume("|") must beEqualTo(Some(State("|"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryOr).toDFA.consume("^") must beEqualTo(None)
      }
    }

    "match binary or assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("|=") must beEqualTo(Some(State("|="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("|") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryOrAssign).toDFA.consume("| ") must beEqualTo(None)
      }
    }

    "match binary and" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryAnd).toDFA.consume("&") must beEqualTo(Some(State("&"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryAnd).toDFA.consume("|") must beEqualTo(None)
      }
    }

    "match binary and assign" in {
      "success" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("&=") must beEqualTo(Some(State("&="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("&") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.BinaryAndAssign).toDFA.consume("& ") must beEqualTo(None)
      }
    }


    "match logical or" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("||") must beEqualTo(Some(State("||"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("|") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("| ") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalOr).toDFA.consume("&&") must beEqualTo(None)
      }
    }

    "match logical and" in {
      "success" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("&&") must beEqualTo(Some(State("&&"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("&") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("& ") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.LogicalAnd).toDFA.consume("||") must beEqualTo(None)
      }
    }

    "match semicolon" in {
      "success" in {
        TokenNFA.nfas(Token.Semicolon).toDFA.consume(";") must beEqualTo(Some(State(";"), ""))
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("\b") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Whitespace).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("/**\n* Hello\n* World\n */") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.SingleLineComment).toDFA.consume("/* Hello World */") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.MultiLineComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("/*\n**/") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.JavaDocComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Num).toDFA.consume("-1") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.Identifier).toDFA.consume("-world") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'abc'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\n'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\r\\n'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\9'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\97'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\79'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\477'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\397'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.CharLiteral).toDFA.consume("'\\379'") must beEqualTo(None)
      }
      "failed as expected" in {
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
      "failed as expected" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"\n\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(Token.StringLiteral).toDFA.consume("") must beEqualTo(None)
      }
    }

    "match keywords" in {
      "public" in {
        TokenNFA.nfas(Token.PublicKeyword).toDFA.consume("public") must beEqualTo(Some(State("c"), ""))
      }
      "static" in {
        TokenNFA.nfas(Token.StaticKeyword).toDFA.consume("static") must beEqualTo(Some(State("c"), ""))
      }
      "boolean" in {
        TokenNFA.nfas(Token.BooleanKeyword).toDFA.consume("boolean") must beEqualTo(Some(State("n"), ""))
      }
    }
  }

  "matchString" in {
    "non-string tokens" in {
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
    }

    "individual keywords" in {
      "abstract" in {
        val nfa = NFA.fromString("abstract", Token.AbstractKeyword("abstract"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("abstract") must beEqualTo(Some(List(Token.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        val nfa = NFA.fromString("boolean", Token.BooleanKeyword("boolean"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("boolean") must beEqualTo(Some(List(Token.BooleanKeyword("boolean"))))
      }

      "break" in {
        val nfa = NFA.fromString("break", Token.BreakKeyword("break"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("break") must beEqualTo(Some(List(Token.BreakKeyword("break"))))
      }

      "byte" in {
        val nfa = NFA.fromString("byte", Token.ByteKeyword("byte"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("byte") must beEqualTo(Some(List(Token.ByteKeyword("byte"))))
      }

      "case" in {
        val nfa = NFA.fromString("case", Token.CaseKeyword("case"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
      }

      "catch" in {
        val nfa = NFA.fromString("catch", Token.CatchKeyword("catch"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
      }

      "char" in {
        val nfa = NFA.fromString("char", Token.CharKeyword("char"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("char") must beEqualTo(Some(List(Token.CharKeyword("char"))))
      }

      "class" in {
        val nfa = NFA.fromString("class", Token.ClassKeyword("class"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("class") must beEqualTo(Some(List(Token.ClassKeyword("class"))))
      }

      "const" in {
        val nfa = NFA.fromString("const", Token.ConstKeyword("const"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("const") must beEqualTo(Some(List(Token.ConstKeyword("const"))))
      }

      "continue" in {
        val nfa = NFA.fromString("continue", Token.ContinueKeyword("continue"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("continue") must beEqualTo(Some(List(Token.ContinueKeyword("continue"))))
      }

      "default" in {
        val nfa = NFA.fromString("default", Token.DefaultKeyword("default"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("default") must beEqualTo(Some(List(Token.DefaultKeyword("default"))))
      }

      "do" in {
        val nfa = NFA.fromString("do", Token.DoKeyword("do"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("do") must beEqualTo(Some(List(Token.DoKeyword("do"))))
      }

      "double" in {
        val nfa = NFA.fromString("double", Token.DoubleKeyword("double"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("double") must beEqualTo(Some(List(Token.DoubleKeyword("double"))))
      }

      "else" in {
        val nfa = NFA.fromString("else", Token.ElseKeyword("else"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("else") must beEqualTo(Some(List(Token.ElseKeyword("else"))))
      }

      "extends" in {
        val nfa = NFA.fromString("extends", Token.ExtendsKeyword("extends"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("extends") must beEqualTo(Some(List(Token.ExtendsKeyword("extends"))))
      }

      "final" in {
        val nfa = NFA.fromString("final", Token.FinalKeyword("final"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("final") must beEqualTo(Some(List(Token.FinalKeyword("final"))))
      }

      "finally" in {
        val nfa = NFA.fromString("finally", Token.FinallyKeyword("finally"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("finally") must beEqualTo(Some(List(Token.FinallyKeyword("finally"))))
      }

      "float" in {
        val nfa = NFA.fromString("float", Token.FloatKeyword("float"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("float") must beEqualTo(Some(List(Token.FloatKeyword("float"))))
      }

      "for" in {
        val nfa = NFA.fromString("for", Token.ForKeyword("for"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("for") must beEqualTo(Some(List(Token.ForKeyword("for"))))
      }

      "goto" in {
        val nfa = NFA.fromString("goto", Token.GotoKeyword("goto"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("goto") must beEqualTo(Some(List(Token.GotoKeyword("goto"))))
      }

      "if" in {
        val nfa = NFA.fromString("if", Token.IfKeyword("if"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("if") must beEqualTo(Some(List(Token.IfKeyword("if"))))
      }

      "implements" in {
        val nfa = NFA.fromString("implements", Token.ImplementsKeyword("implements"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("implements") must beEqualTo(Some(List(Token.ImplementsKeyword("implements"))))
      }

      "import" in {
        val nfa = NFA.fromString("import", Token.ImportKeyword("import"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("import") must beEqualTo(Some(List(Token.ImportKeyword("import"))))
      }

      "instanceof" in {
        val nfa = NFA.fromString("instanceof", Token.InstanceofKeyword("instanceof"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("instanceof") must beEqualTo(Some(List(Token.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        val nfa = NFA.fromString("int", Token.IntKeyword("int"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("int") must beEqualTo(Some(List(Token.IntKeyword("int"))))
      }

      "interface" in {
        val nfa = NFA.fromString("interface", Token.InterfaceKeyword("interface"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("interface") must beEqualTo(Some(List(Token.InterfaceKeyword("interface"))))
      }

      "long" in {
        val nfa = NFA.fromString("long", Token.LongKeyword("long"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("long") must beEqualTo(Some(List(Token.LongKeyword("long"))))
      }

      "native" in {
        val nfa = NFA.fromString("native", Token.NativeKeyword("native"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("native") must beEqualTo(Some(List(Token.NativeKeyword("native"))))
      }

      "new" in {
        val nfa = NFA.fromString("new", Token.NewKeyword("new"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("new") must beEqualTo(Some(List(Token.NewKeyword("new"))))
      }

      "package" in {
        val nfa = NFA.fromString("package", Token.PackageKeyword("package"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("package") must beEqualTo(Some(List(Token.PackageKeyword("package"))))
      }

      "private" in {
        val nfa = NFA.fromString("private", Token.PrivateKeyword("private"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("private") must beEqualTo(Some(List(Token.PrivateKeyword("private"))))
      }

      "protected" in {
        val nfa = NFA.fromString("protected", Token.ProtectedKeyword("protected"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("protected") must beEqualTo(Some(List(Token.ProtectedKeyword("protected"))))
      }

      "public" in {
        val nfa = NFA.fromString("public", Token.PublicKeyword("public"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("public") must beEqualTo(Some(List(Token.PublicKeyword("public"))))
      }

      "return" in {
        val nfa = NFA.fromString("return", Token.ReturnKeyword("return"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("return") must beEqualTo(Some(List(Token.ReturnKeyword("return"))))
      }

      "short" in {
        val nfa = NFA.fromString("short", Token.ShortKeyword("short"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("short") must beEqualTo(Some(List(Token.ShortKeyword("short"))))
      }

      "static" in {
        val nfa = NFA.fromString("static", Token.StaticKeyword("static"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("static") must beEqualTo(Some(List(Token.StaticKeyword("static"))))
      }

      "strictfp" in {
        val nfa = NFA.fromString("strictfp", Token.StrictfpKeyword("strictfp"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("strictfp") must beEqualTo(Some(List(Token.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        val nfa = NFA.fromString("super", Token.SuperKeyword("super"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("super") must beEqualTo(Some(List(Token.SuperKeyword("super"))))
      }

      "switch" in {
        val nfa = NFA.fromString("switch", Token.SwitchKeyword("switch"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("switch") must beEqualTo(Some(List(Token.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        val nfa = NFA.fromString("synchronized", Token.SynchronizedKeyword("synchronized"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("synchronized") must beEqualTo(Some(List(Token.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        val nfa = NFA.fromString("this", Token.ThisKeyword("this"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("this") must beEqualTo(Some(List(Token.ThisKeyword("this"))))
      }

      "throw" in {
        val nfa = NFA.fromString("throw", Token.ThrowKeyword("throw"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("throw") must beEqualTo(Some(List(Token.ThrowKeyword("throw"))))
      }

      "throws" in {
        val nfa = NFA.fromString("throws", Token.ThrowsKeyword("throws"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("throws") must beEqualTo(Some(List(Token.ThrowsKeyword("throws"))))
      }

      "transient" in {
        val nfa = NFA.fromString("transient", Token.TransientKeyword("transient"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("transient") must beEqualTo(Some(List(Token.TransientKeyword("transient"))))
      }

      "try" in {
        val nfa = NFA.fromString("try", Token.TryKeyword("try"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("try") must beEqualTo(Some(List(Token.TryKeyword("try"))))
      }

      "void" in {
        val nfa = NFA.fromString("void", Token.VoidKeyword("void"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("void") must beEqualTo(Some(List(Token.VoidKeyword("void"))))
      }

      "volatile" in {
        val nfa = NFA.fromString("volatile", Token.VolatileKeyword("volatile"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("volatile") must beEqualTo(Some(List(Token.VolatileKeyword("volatile"))))
      }

      "while" in {
        val nfa = NFA.fromString("while", Token.WhileKeyword("while"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("while") must beEqualTo(Some(List(Token.WhileKeyword("while"))))
      }


      "true" in {
        val nfa = NFA.fromString("true", Token.TrueLiteral("true"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("true") must beEqualTo(Some(List(Token.TrueLiteral("true"))))
      }

      "false" in {
        val nfa = NFA.fromString("false", Token.FalseLiteral("false"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("false") must beEqualTo(Some(List(Token.FalseLiteral("false"))))
      }

      "null" in {
        val nfa = NFA.fromString("null", Token.NullLiteral("null"))
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(Token.Identifier))).toDFA
        matchAgainst.matchString("null") must beEqualTo(Some(List(Token.NullLiteral("null"))))
      }
    }

    "keyword union" in {
      val keywordsNFAWithoutIdentifier: Set[NFA] = Token.Keywords.map {
        case (keyword: String, token) => NFA.fromString(keyword, token(keyword))
      }.toSet

      val keywordsNFA: NFA = NFA.union(keywordsNFAWithoutIdentifier + TokenNFA.nfas(Token.Identifier))
      val keywordsDFA: DFA = keywordsNFA.toDFA

      "abstract" in {
        keywordsDFA.matchString("abstract") must beEqualTo(Some(List(Token.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        keywordsDFA.matchString("boolean") must beEqualTo(Some(List(Token.BooleanKeyword("boolean"))))
      }

      "break" in {
        keywordsDFA.matchString("break") must beEqualTo(Some(List(Token.BreakKeyword("break"))))
      }

      "byte" in {
        keywordsDFA.matchString("byte") must beEqualTo(Some(List(Token.ByteKeyword("byte"))))
      }

      "case" in {
        keywordsDFA.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
      }

      "catch" in {
        keywordsDFA.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
      }

      "char" in {
        keywordsDFA.matchString("char") must beEqualTo(Some(List(Token.CharKeyword("char"))))
      }

      "class" in {
        keywordsDFA.matchString("class") must beEqualTo(Some(List(Token.ClassKeyword("class"))))
      }

      "const" in {
        keywordsDFA.matchString("const") must beEqualTo(Some(List(Token.ConstKeyword("const"))))
      }

      "continue" in {
        keywordsDFA.matchString("continue") must beEqualTo(Some(List(Token.ContinueKeyword("continue"))))
      }

      "default" in {
        keywordsDFA.matchString("default") must beEqualTo(Some(List(Token.DefaultKeyword("default"))))
      }

      "do" in {
        keywordsDFA.matchString("do") must beEqualTo(Some(List(Token.DoKeyword("do"))))
      }

      "double" in {
        keywordsDFA.matchString("double") must beEqualTo(Some(List(Token.DoubleKeyword("double"))))
      }

      "else" in {
        keywordsDFA.matchString("else") must beEqualTo(Some(List(Token.ElseKeyword("else"))))
      }

      "extends" in {
        keywordsDFA.matchString("extends") must beEqualTo(Some(List(Token.ExtendsKeyword("extends"))))
      }

      "final" in {
        keywordsDFA.matchString("final") must beEqualTo(Some(List(Token.FinalKeyword("final"))))
      }

      "finally" in {
        keywordsDFA.matchString("finally") must beEqualTo(Some(List(Token.FinallyKeyword("finally"))))
      }

      "float" in {
        keywordsDFA.matchString("float") must beEqualTo(Some(List(Token.FloatKeyword("float"))))
      }

      "for" in {
        keywordsDFA.matchString("for") must beEqualTo(Some(List(Token.ForKeyword("for"))))
      }

      "goto" in {
        keywordsDFA.matchString("goto") must beEqualTo(Some(List(Token.GotoKeyword("goto"))))
      }

      "if" in {
        keywordsDFA.matchString("if") must beEqualTo(Some(List(Token.IfKeyword("if"))))
      }

      "implements" in {
        keywordsDFA.matchString("implements") must beEqualTo(Some(List(Token.ImplementsKeyword("implements"))))
      }

      "import" in {
        keywordsDFA.matchString("import") must beEqualTo(Some(List(Token.ImportKeyword("import"))))
      }

      "instanceof" in {
        keywordsDFA.matchString("instanceof") must beEqualTo(Some(List(Token.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        keywordsDFA.matchString("int") must beEqualTo(Some(List(Token.IntKeyword("int"))))
      }

      "interface" in {
        keywordsDFA.matchString("interface") must beEqualTo(Some(List(Token.InterfaceKeyword("interface"))))
      }

      "long" in {
        keywordsDFA.matchString("long") must beEqualTo(Some(List(Token.LongKeyword("long"))))
      }

      "native" in {
        keywordsDFA.matchString("native") must beEqualTo(Some(List(Token.NativeKeyword("native"))))
      }

      "new" in {
        keywordsDFA.matchString("new") must beEqualTo(Some(List(Token.NewKeyword("new"))))
      }

      "package" in {
        keywordsDFA.matchString("package") must beEqualTo(Some(List(Token.PackageKeyword("package"))))
      }

      "private" in {
        keywordsDFA.matchString("private") must beEqualTo(Some(List(Token.PrivateKeyword("private"))))
      }

      "protected" in {
        keywordsDFA.matchString("protected") must beEqualTo(Some(List(Token.ProtectedKeyword("protected"))))
      }

      "public" in {
        keywordsDFA.matchString("public") must beEqualTo(Some(List(Token.PublicKeyword("public"))))
      }

      "return" in {
        keywordsDFA.matchString("return") must beEqualTo(Some(List(Token.ReturnKeyword("return"))))
      }

      "short" in {
        keywordsDFA.matchString("short") must beEqualTo(Some(List(Token.ShortKeyword("short"))))
      }

      "static" in {
        keywordsDFA.matchString("static") must beEqualTo(Some(List(Token.StaticKeyword("static"))))
      }

      "strictfp" in {
        keywordsDFA.matchString("strictfp") must beEqualTo(Some(List(Token.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        keywordsDFA.matchString("super") must beEqualTo(Some(List(Token.SuperKeyword("super"))))
      }

      "switch" in {
        keywordsDFA.matchString("switch") must beEqualTo(Some(List(Token.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        keywordsDFA.matchString("synchronized") must beEqualTo(Some(List(Token.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        keywordsDFA.matchString("this") must beEqualTo(Some(List(Token.ThisKeyword("this"))))
      }

      "throw" in {
        keywordsDFA.matchString("throw") must beEqualTo(Some(List(Token.ThrowKeyword("throw"))))
      }

      "throws" in {
        keywordsDFA.matchString("throws") must beEqualTo(Some(List(Token.ThrowsKeyword("throws"))))
      }

      "transient" in {
        keywordsDFA.matchString("transient") must beEqualTo(Some(List(Token.TransientKeyword("transient"))))
      }

      "try" in {
        keywordsDFA.matchString("try") must beEqualTo(Some(List(Token.TryKeyword("try"))))
      }

      "void" in {
        keywordsDFA.matchString("void") must beEqualTo(Some(List(Token.VoidKeyword("void"))))
      }

      "volatile" in {
        keywordsDFA.matchString("volatile") must beEqualTo(Some(List(Token.VolatileKeyword("volatile"))))
      }

      "while" in {
        keywordsDFA.matchString("while") must beEqualTo(Some(List(Token.WhileKeyword("while"))))
      }


      "true" in {
        keywordsDFA.matchString("true") must beEqualTo(Some(List(Token.TrueLiteral("true"))))
      }

      "false" in {
        keywordsDFA.matchString("false") must beEqualTo(Some(List(Token.FalseLiteral("false"))))
      }

      "null" in {
        keywordsDFA.matchString("null") must beEqualTo(Some(List(Token.NullLiteral("null"))))
      }
    }

    "similar prefixes" in {
      "case, catch" in {
        val keywordsNFA: NFA = NFA.union(Set(
          TokenNFA.nfas(Token.CaseKeyword),
          TokenNFA.nfas(Token.CatchKeyword),
          TokenNFA.nfas(Token.Identifier)
        ))
        val keywordsDFA: DFA = keywordsNFA.toDFA

        println(keywordsNFA.toGraphViz)
        println(keywordsDFA.toGraphViz)

        keywordsDFA.matchString("case") must beEqualTo(Some(List(Token.CaseKeyword("case"))))
        keywordsDFA.matchString("catch") must beEqualTo(Some(List(Token.CatchKeyword("catch"))))
        keywordsDFA.matchString("neither") must beEqualTo(Some(List(Token.Identifier("neither"))))
      }
    }
/*
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
          Token.Identifier("int"),
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
    }*/
  }
}
