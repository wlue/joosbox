package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._
import InputStringImplicits._

class TokenSpec extends Specification {
  "Token" should {
    "match question" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Question).toDFA.consume("?") must beEqualTo(Some(State("?"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Question).toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match parens" in {
      "leftparen success" in {
        TokenNFA.nfas(TokenTypes.LeftParen).toDFA.consume("(") must beEqualTo(Some(State("("), ""))
      }
      "leftparen failure" in {
        TokenNFA.nfas(TokenTypes.LeftParen).toDFA.consume(")") must beEqualTo(None)
      }
      "rightparen success" in {
        TokenNFA.nfas(TokenTypes.RightParen).toDFA.consume(")") must beEqualTo(Some(State(")"), ""))
      }
      "rightparen failure" in {
        TokenNFA.nfas(TokenTypes.RightParen).toDFA.consume("(") must beEqualTo(None)
      }
    }

    "match brackets" in {
      "leftbracket success" in {
        TokenNFA.nfas(TokenTypes.LeftBracket).toDFA.consume("[") must beEqualTo(Some(State("["), ""))
      }
      "leftbracket failure" in {
        TokenNFA.nfas(TokenTypes.LeftBracket).toDFA.consume("]") must beEqualTo(None)
      }
      "rightbracket success" in {
        TokenNFA.nfas(TokenTypes.RightBracket).toDFA.consume("]") must beEqualTo(Some(State("]"), ""))
      }
      "rightbracket failure" in {
        TokenNFA.nfas(TokenTypes.RightBracket).toDFA.consume("[") must beEqualTo(None)
      }
    }

    "match curlies" in {
      "leftcurly success" in {
        TokenNFA.nfas(TokenTypes.LeftCurly).toDFA.consume("{") must beEqualTo(Some(State("{"), ""))
      }
      "leftcurly failure" in {
        TokenNFA.nfas(TokenTypes.LeftCurly).toDFA.consume("(") must beEqualTo(None)
      }
      "rightcurly success" in {
        TokenNFA.nfas(TokenTypes.RightCurly).toDFA.consume("}") must beEqualTo(Some(State("}"), ""))
      }
      "rightcurly failure" in {
        TokenNFA.nfas(TokenTypes.RightCurly).toDFA.consume("]") must beEqualTo(None)
      }
    }

    "match colon" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Colon).toDFA.consume(":") must beEqualTo(Some(State(":"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Colon).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match comma" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Comma).toDFA.consume(",") must beEqualTo(Some(State(","), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Comma).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match dot" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Dot).toDFA.consume(".") must beEqualTo(Some(State("."), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Dot).toDFA.consume(";") must beEqualTo(None)
      }
    }

    "match assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Assign).toDFA.consume("=") must beEqualTo(Some(State("="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Assign).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match equal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Equal).toDFA.consume("==") must beEqualTo(Some(State("=="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Equal).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Equal).toDFA.consume(",") must beEqualTo(None)
      }
    }

    "match logical not" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.LogicalNot).toDFA.consume("!") must beEqualTo(Some(State("!"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalNot).toDFA.consume("~") must beEqualTo(None)
      }
    }

    "match binary not" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryNot).toDFA.consume("~") must beEqualTo(Some(State("~"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryNot).toDFA.consume("!") must beEqualTo(None)
      }
    }

    "match not equal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.NotEqual).toDFA.consume("!=") must beEqualTo(Some(State("!="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.NotEqual).toDFA.consume("!") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.NotEqual).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.NotEqual).toDFA.consume("==") must beEqualTo(None)
      }
    }

    "match divide" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Divide).toDFA.consume("/") must beEqualTo(Some(State("/"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Divide).toDFA.consume("\\") must beEqualTo(None)
      }
    }

    "match divide-assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.DivideAssign).toDFA.consume("/=") must beEqualTo(Some(State("/="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.DivideAssign).toDFA.consume("\\=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.DivideAssign).toDFA.consume("/") must beEqualTo(None)
      }
    }

    "match plus" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Plus).toDFA.consume("+") must beEqualTo(Some(State("+"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Plus).toDFA.consume("-") must beEqualTo(None)
      }
    }

    "match plus-assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.PlusAssign).toDFA.consume("+=") must beEqualTo(Some(State("+="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.PlusAssign).toDFA.consume("+") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match increment" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Increment).toDFA.consume("++") must beEqualTo(Some(State("++"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Increment).toDFA.consume("+") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Increment).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match minus" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Minus).toDFA.consume("-") must beEqualTo(Some(State("-"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Minus).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match minus-assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.MinusAssign).toDFA.consume("-=") must beEqualTo(Some(State("-="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.PlusAssign).toDFA.consume("-") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.PlusAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match decrement" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Decrement).toDFA.consume("--") must beEqualTo(Some(State("--"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Decrement).toDFA.consume("-") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Decrement).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Star).toDFA.consume("*") must beEqualTo(Some(State("*"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Star).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match star-assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.StarAssign).toDFA.consume("*=") must beEqualTo(Some(State("*="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StarAssign).toDFA.consume("*") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StarAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match modulo" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Modulo).toDFA.consume("%") must beEqualTo(Some(State("%"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Modulo).toDFA.consume("+") must beEqualTo(None)
      }
    }

    "match modulo-assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.ModuloAssign).toDFA.consume("%=") must beEqualTo(Some(State("%="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ModuloAssign).toDFA.consume("%") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ModuloAssign).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match greater than" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.GreaterThan).toDFA.consume(">") must beEqualTo(Some(State(">"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.GreaterThan).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match greater than equal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.GreaterEqual).toDFA.consume(">=") must beEqualTo(Some(State(">="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.GreaterEqual).toDFA.consume(">") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.GreaterEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift right" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.ShiftRight).toDFA.consume(">>") must beEqualTo(Some(State(">>"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftRight).toDFA.consume(">=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftRight).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match shift right assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.ShiftRightAssign).toDFA.consume(">>=") must beEqualTo(Some(State(">>="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftRightAssign).toDFA.consume(">=") must beEqualTo(None)
      }
    }

    "match binary shift right" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRight).toDFA.consume(">>>") must beEqualTo(Some(State(">>>"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRight).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRight).toDFA.consume(">>") must beEqualTo(None)
      }
    }

    "match binary shift right assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRightAssign).toDFA.consume(">>>=") must beEqualTo(Some(State(">>>="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRightAssign).toDFA.consume(">>>") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRightAssign).toDFA.consume(">>=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryShiftRightAssign).toDFA.consume(">>") must beEqualTo(None)
      }
    }

    "match less than" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.LessThan).toDFA.consume("<") must beEqualTo(Some(State("<"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LessThan).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match less than equal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.LessEqual).toDFA.consume("<=") must beEqualTo(Some(State("<="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LessEqual).toDFA.consume("<") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LessEqual).toDFA.consume("=") must beEqualTo(None)
      }
    }

    "match shift left" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.ShiftLeft).toDFA.consume("<<") must beEqualTo(Some(State("<<"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftLeft).toDFA.consume("<=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftLeft).toDFA.consume("<") must beEqualTo(None)
      }
    }

    "match shift left assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.ShiftLeftAssign).toDFA.consume("<<=") must beEqualTo(Some(State("<<="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftLeftAssign).toDFA.consume("<<") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.ShiftLeftAssign).toDFA.consume("<=") must beEqualTo(None)
      }
    }

    "match binary xor" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryXor).toDFA.consume("^") must beEqualTo(Some(State("^"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryXor).toDFA.consume(">") must beEqualTo(None)
      }
    }

    "match binary xor assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryXorAssign).toDFA.consume("^=") must beEqualTo(Some(State("^="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryXorAssign).toDFA.consume("^") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryXorAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryXorAssign).toDFA.consume("^ ") must beEqualTo(None)
      }
    }


    "match binary or" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryOr).toDFA.consume("|") must beEqualTo(Some(State("|"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryOr).toDFA.consume("^") must beEqualTo(None)
      }
    }

    "match binary or assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryOrAssign).toDFA.consume("|=") must beEqualTo(Some(State("|="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryOrAssign).toDFA.consume("|") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryOrAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryOrAssign).toDFA.consume("| ") must beEqualTo(None)
      }
    }

    "match binary and" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryAnd).toDFA.consume("&") must beEqualTo(Some(State("&"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryAnd).toDFA.consume("|") must beEqualTo(None)
      }
    }

    "match binary and assign" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.BinaryAndAssign).toDFA.consume("&=") must beEqualTo(Some(State("&="), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryAndAssign).toDFA.consume("&") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryAndAssign).toDFA.consume("=") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.BinaryAndAssign).toDFA.consume("& ") must beEqualTo(None)
      }
    }


    "match logical or" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.LogicalOr).toDFA.consume("||") must beEqualTo(Some(State("||"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalOr).toDFA.consume("|") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalOr).toDFA.consume("| ") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalOr).toDFA.consume("&&") must beEqualTo(None)
      }
    }

    "match logical and" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.LogicalAnd).toDFA.consume("&&") must beEqualTo(Some(State("&&"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalAnd).toDFA.consume("&") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalAnd).toDFA.consume("& ") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.LogicalAnd).toDFA.consume("||") must beEqualTo(None)
      }
    }

    "match semicolon" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Semicolon).toDFA.consume(";") must beEqualTo(Some(State(";"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Semicolon).toDFA.consume("|") must beEqualTo(None)
      }
    }

    "match whitespace" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume(" ") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\r") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\t") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\f") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\13") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\r\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\n\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\r\r") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\r\r\n") must beEqualTo(Some(State("ws"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("      ") must beEqualTo(Some(State("ws"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("\b") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Whitespace).toDFA.consume("a  ") must beEqualTo(None)
      }
    }

    "match single line comment" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("//\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello World\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello // World\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("//\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello World\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello // World\r") must beEqualTo(Some(State("eol2"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("//\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello World\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("// Hello // World\r\n") must beEqualTo(Some(State("eol"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("/**\n* Hello\n* World\n */") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("/* Hello World */") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.SingleLineComment).toDFA.consume("Hello // World\n") must beEqualTo(None)
      }
    }

    "match multiline comment" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/* Hello World */") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/* Hello World\n*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/* Hello\n World*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/* Hello // World*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/**\n* Hello\n*  World\n */") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/**/") must beEqualTo(Some(State("/**/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("/*\n*/") must beEqualTo(Some(State("/**/"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.MultiLineComment).toDFA.consume("Hello /* World */") must beEqualTo(None)
      }
    }

    "match javadoc comment" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/** Hello World */") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/** Hello World\n*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/** Hello\n World*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/** Hello // World*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/**\n* Hello\n*  World\n */") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/***/") must beEqualTo(Some(State("/***/"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/**\n*/") must beEqualTo(Some(State("/***/"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("/*\n**/") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("//") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.JavaDocComment).toDFA.consume("Hello /** World */") must beEqualTo(None)
      }
    }

    "match number" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("0") must beEqualTo(Some(State("zero"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("1") must beEqualTo(Some(State("digit"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("10") must beEqualTo(Some(State("digit"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("1234567890") must beEqualTo(Some(State("digit"), ""))
      }
      "no leading zeros" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("0345") must beEqualTo(Some(State("zero"), "345"))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("-1") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Num).toDFA.consume("one") must beEqualTo(None)
      }
    }

    "match identifier" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("a") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("abc") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("AbCdEf") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("abc_def$") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("$hello") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("_hello") must beEqualTo(Some(State("id"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("world1") must beEqualTo(Some(State("id"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("-world") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.Identifier).toDFA.consume("1world") must beEqualTo(None)
      }
    }

    "match char literal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'a'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'0'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'^'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("' '") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\"'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\n'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\b'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\\''") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\0'") must beEqualTo(Some(State("char"), ""))
      }
       "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\7'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\77'") must beEqualTo(Some(State("char"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\377'") must beEqualTo(Some(State("char"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'abc'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\n'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\r\\n'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\9'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\97'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\79'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\477'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\397'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\379'") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.CharLiteral).toDFA.consume("'\\3771'") must beEqualTo(None)
      }
    }

    "match string literal" in {
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"a\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"abc\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"Hello, world!\\n\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\\"Nested\\\"\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\0\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\09\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\377Hello\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"123\\45678\"") must beEqualTo(Some(State("string"), ""))
      }
      "success" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"Joos\' World\"") must beEqualTo(Some(State("string"), ""))
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\80\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\9\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\u1234\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\\z\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"\n\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("\"") must beEqualTo(None)
      }
      "failed as expected" in {
        TokenNFA.nfas(TokenTypes.StringLiteral).toDFA.consume("") must beEqualTo(None)
      }
    }

    "match keywords" in {
      "public" in {
        TokenNFA.nfas(TokenTypes.PublicKeyword).toDFA.consume("public") must beEqualTo(Some(State("public"), ""))
      }
      "static" in {
        TokenNFA.nfas(TokenTypes.StaticKeyword).toDFA.consume("static") must beEqualTo(Some(State("static"), ""))
      }
      "boolean" in {
        TokenNFA.nfas(TokenTypes.BooleanKeyword).toDFA.consume("boolean") must beEqualTo(Some(State("boolean"), ""))
      }
    }
  }

  "matchString" in {
    "non-string tokens" in {
      "simple assign/equals matching" in {
        val testNFAs = Set[NFA](TokenNFA.nfas(TokenTypes.Assign), TokenNFA.nfas(TokenTypes.Equal))
        val mergedTestDFA = NFA.union(testNFAs).toDFA

        mergedTestDFA.matchString("=") must beEqualTo(Some(List(Tokens.Assign("="))))
        mergedTestDFA.matchString("==") must beEqualTo(Some(List(Tokens.Equal("=="))))
        mergedTestDFA.matchString("===") must beEqualTo(Some(List(Tokens.Equal("=="), Tokens.Assign(("=", 2)))))
        mergedTestDFA.matchString("+") must beEqualTo(None)
      }

      "simple assign/equal/not matching" in {
        val testNFAs = Set[NFA](
          TokenNFA.nfas(TokenTypes.Assign),
          TokenNFA.nfas(TokenTypes.Equal),
          TokenNFA.nfas(TokenTypes.LogicalNot)
        )
        val mergedTestDFA = NFA.union(testNFAs).toDFA

        mergedTestDFA.matchString("=") must beEqualTo(Some(List(Tokens.Assign("="))))
        mergedTestDFA.matchString("==") must beEqualTo(Some(List(Tokens.Equal("=="))))
        mergedTestDFA.matchString("!") must beEqualTo(Some(List(TokenTypes.LogicalNot.create("!"))))
      }

      "simple assign/equal/not/notequal matching" in {
        val testNFAs = Set[NFA](
          TokenNFA.nfas(TokenTypes.Assign),
          TokenNFA.nfas(TokenTypes.Equal),
          TokenNFA.nfas(TokenTypes.LogicalNot),
          TokenNFA.nfas(TokenTypes.NotEqual)        
        )
        val mergedTestDFA = NFA.union(testNFAs).toDFA

        mergedTestDFA.matchString("=") must beEqualTo(Some(List(Tokens.Assign("="))))
        mergedTestDFA.matchString("==") must beEqualTo(Some(List(Tokens.Equal("=="))))
        mergedTestDFA.matchString("!") must beEqualTo(Some(List(TokenTypes.LogicalNot.create("!"))))
        mergedTestDFA.matchString("!=") must beEqualTo(Some(List(TokenTypes.NotEqual.create("!="))))
      }
    }

    "individual keywords" in {
      "abstract" in {
        val nfa = NFA.fromString("abstract", TokenTypes.AbstractKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("abstract") must beEqualTo(Some(List(Tokens.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        val nfa = NFA.fromString("boolean", TokenTypes.BooleanKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("boolean") must beEqualTo(Some(List(Tokens.BooleanKeyword("boolean"))))
      }

      "break" in {
        val nfa = NFA.fromString("break", TokenTypes.BreakKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("break") must beEqualTo(Some(List(Tokens.BreakKeyword("break"))))
      }

      "byte" in {
        val nfa = NFA.fromString("byte", TokenTypes.ByteKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("byte") must beEqualTo(Some(List(Tokens.ByteKeyword("byte"))))
      }

      "case" in {
        val nfa = NFA.fromString("case", TokenTypes.CaseKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
      }

      "catch" in {
        val nfa = NFA.fromString("catch", TokenTypes.CatchKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
      }

      "char" in {
        val nfa = NFA.fromString("char", TokenTypes.CharKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("char") must beEqualTo(Some(List(Tokens.CharKeyword("char"))))
      }

      "class" in {
        val nfa = NFA.fromString("class", TokenTypes.ClassKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("class") must beEqualTo(Some(List(Tokens.ClassKeyword("class"))))
      }

      "const" in {
        val nfa = NFA.fromString("const", TokenTypes.ConstKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("const") must beEqualTo(Some(List(Tokens.ConstKeyword("const"))))
      }

      "continue" in {
        val nfa = NFA.fromString("continue", TokenTypes.ContinueKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("continue") must beEqualTo(Some(List(Tokens.ContinueKeyword("continue"))))
      }

      "default" in {
        val nfa = NFA.fromString("default", TokenTypes.DefaultKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("default") must beEqualTo(Some(List(Tokens.DefaultKeyword("default"))))
      }

      "do" in {
        val nfa = NFA.fromString("do", TokenTypes.DoKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("do") must beEqualTo(Some(List(Tokens.DoKeyword("do"))))
      }

      "double" in {
        val nfa = NFA.fromString("double", TokenTypes.DoubleKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("double") must beEqualTo(Some(List(Tokens.DoubleKeyword("double"))))
      }

      "else" in {
        val nfa = NFA.fromString("else", TokenTypes.ElseKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("else") must beEqualTo(Some(List(Tokens.ElseKeyword("else"))))
      }

      "extends" in {
        val nfa = NFA.fromString("extends", TokenTypes.ExtendsKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("extends") must beEqualTo(Some(List(Tokens.ExtendsKeyword("extends"))))
      }

      "final" in {
        val nfa = NFA.fromString("final", TokenTypes.FinalKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("final") must beEqualTo(Some(List(Tokens.FinalKeyword("final"))))
      }

      "finally" in {
        val nfa = NFA.fromString("finally", TokenTypes.FinallyKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("finally") must beEqualTo(Some(List(Tokens.FinallyKeyword("finally"))))
      }

      "float" in {
        val nfa = NFA.fromString("float", TokenTypes.FloatKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("float") must beEqualTo(Some(List(Tokens.FloatKeyword("float"))))
      }

      "for" in {
        val nfa = NFA.fromString("for", TokenTypes.ForKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("for") must beEqualTo(Some(List(Tokens.ForKeyword("for"))))
      }

      "goto" in {
        val nfa = NFA.fromString("goto", TokenTypes.GotoKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("goto") must beEqualTo(Some(List(Tokens.GotoKeyword("goto"))))
      }

      "if" in {
        val nfa = NFA.fromString("if", TokenTypes.IfKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("if") must beEqualTo(Some(List(Tokens.IfKeyword("if"))))
      }

      "implements" in {
        val nfa = NFA.fromString("implements", TokenTypes.ImplementsKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("implements") must beEqualTo(Some(List(Tokens.ImplementsKeyword("implements"))))
      }

      "import" in {
        val nfa = NFA.fromString("import", TokenTypes.ImportKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("import") must beEqualTo(Some(List(Tokens.ImportKeyword("import"))))
      }

      "instanceof" in {
        val nfa = NFA.fromString("instanceof", TokenTypes.InstanceofKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("instanceof") must beEqualTo(Some(List(Tokens.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        val nfa = NFA.fromString("int", TokenTypes.IntKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("int") must beEqualTo(Some(List(Tokens.IntKeyword("int"))))
      }

      "interface" in {
        val nfa = NFA.fromString("interface", TokenTypes.InterfaceKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("interface") must beEqualTo(Some(List(Tokens.InterfaceKeyword("interface"))))
      }

      "long" in {
        val nfa = NFA.fromString("long", TokenTypes.LongKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("long") must beEqualTo(Some(List(Tokens.LongKeyword("long"))))
      }

      "native" in {
        val nfa = NFA.fromString("native", TokenTypes.NativeKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("native") must beEqualTo(Some(List(Tokens.NativeKeyword("native"))))
      }

      "new" in {
        val nfa = NFA.fromString("new", TokenTypes.NewKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("new") must beEqualTo(Some(List(Tokens.NewKeyword("new"))))
      }

      "package" in {
        val nfa = NFA.fromString("package", TokenTypes.PackageKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("package") must beEqualTo(Some(List(Tokens.PackageKeyword("package"))))
      }

      "private" in {
        val nfa = NFA.fromString("private", TokenTypes.PrivateKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("private") must beEqualTo(Some(List(Tokens.PrivateKeyword("private"))))
      }

      "protected" in {
        val nfa = NFA.fromString("protected", TokenTypes.ProtectedKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("protected") must beEqualTo(Some(List(Tokens.ProtectedKeyword("protected"))))
      }

      "public" in {
        val nfa = NFA.fromString("public", TokenTypes.PublicKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("public") must beEqualTo(Some(List(Tokens.PublicKeyword("public"))))
      }

      "return" in {
        val nfa = NFA.fromString("return", TokenTypes.ReturnKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("return") must beEqualTo(Some(List(Tokens.ReturnKeyword("return"))))
      }

      "short" in {
        val nfa = NFA.fromString("short", TokenTypes.ShortKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("short") must beEqualTo(Some(List(Tokens.ShortKeyword("short"))))
      }

      "static" in {
        val nfa = NFA.fromString("static", TokenTypes.StaticKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("static") must beEqualTo(Some(List(Tokens.StaticKeyword("static"))))
      }

      "strictfp" in {
        val nfa = NFA.fromString("strictfp", TokenTypes.StrictfpKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("strictfp") must beEqualTo(Some(List(Tokens.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        val nfa = NFA.fromString("super", TokenTypes.SuperKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("super") must beEqualTo(Some(List(Tokens.SuperKeyword("super"))))
      }

      "switch" in {
        val nfa = NFA.fromString("switch", TokenTypes.SwitchKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("switch") must beEqualTo(Some(List(Tokens.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        val nfa = NFA.fromString("synchronized", TokenTypes.SynchronizedKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("synchronized") must beEqualTo(Some(List(Tokens.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        val nfa = NFA.fromString("this", TokenTypes.ThisKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("this") must beEqualTo(Some(List(Tokens.ThisKeyword("this"))))
      }

      "throw" in {
        val nfa = NFA.fromString("throw", TokenTypes.ThrowKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("throw") must beEqualTo(Some(List(Tokens.ThrowKeyword("throw"))))
      }

      "throws" in {
        val nfa = NFA.fromString("throws", TokenTypes.ThrowsKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("throws") must beEqualTo(Some(List(Tokens.ThrowsKeyword("throws"))))
      }

      "transient" in {
        val nfa = NFA.fromString("transient", TokenTypes.TransientKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("transient") must beEqualTo(Some(List(Tokens.TransientKeyword("transient"))))
      }

      "try" in {
        val nfa = NFA.fromString("try", TokenTypes.TryKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("try") must beEqualTo(Some(List(Tokens.TryKeyword("try"))))
      }

      "void" in {
        val nfa = NFA.fromString("void", TokenTypes.VoidKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("void") must beEqualTo(Some(List(Tokens.VoidKeyword("void"))))
      }

      "volatile" in {
        val nfa = NFA.fromString("volatile", TokenTypes.VolatileKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("volatile") must beEqualTo(Some(List(Tokens.VolatileKeyword("volatile"))))
      }

      "while" in {
        val nfa = NFA.fromString("while", TokenTypes.WhileKeyword)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("while") must beEqualTo(Some(List(Tokens.WhileKeyword("while"))))
      }


      "true" in {
        val nfa = NFA.fromString("true", TokenTypes.TrueLiteral)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("true") must beEqualTo(Some(List(Tokens.TrueLiteral("true"))))
      }

      "false" in {
        val nfa = NFA.fromString("false", TokenTypes.FalseLiteral)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("false") must beEqualTo(Some(List(Tokens.FalseLiteral("false"))))
      }

      "null" in {
        val nfa = NFA.fromString("null", TokenTypes.NullLiteral)
        val matchAgainst = NFA.union(Set(nfa, TokenNFA.nfas(TokenTypes.Identifier))).toDFA
        matchAgainst.matchString("null") must beEqualTo(Some(List(Tokens.NullLiteral("null"))))
      }
    }

    "keyword union" in {
      val keywordsNFAWithoutIdentifier: Set[NFA] = TokenTypes.Keywords.map {
        case (keyword: String, token: TokenType) => NFA.fromString(keyword, token)
      }.toSet

      val keywordsNFA: NFA = NFA.union(keywordsNFAWithoutIdentifier + TokenNFA.nfas(TokenTypes.Identifier))
      val keywordsDFA: DFA = keywordsNFA.toDFA

      "abstract" in {
        keywordsDFA.matchString("abstract") must beEqualTo(Some(List(Tokens.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        keywordsDFA.matchString("boolean") must beEqualTo(Some(List(Tokens.BooleanKeyword("boolean"))))
      }

      "break" in {
        keywordsDFA.matchString("break") must beEqualTo(Some(List(Tokens.BreakKeyword("break"))))
      }

      "byte" in {
        keywordsDFA.matchString("byte") must beEqualTo(Some(List(Tokens.ByteKeyword("byte"))))
      }

      "case" in {
        keywordsDFA.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
      }

      "catch" in {
        keywordsDFA.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
      }

      "char" in {
        keywordsDFA.matchString("char") must beEqualTo(Some(List(Tokens.CharKeyword("char"))))
      }

      "class" in {
        keywordsDFA.matchString("class") must beEqualTo(Some(List(Tokens.ClassKeyword("class"))))
      }

      "const" in {
        keywordsDFA.matchString("const") must beEqualTo(Some(List(Tokens.ConstKeyword("const"))))
      }

      "continue" in {
        keywordsDFA.matchString("continue") must beEqualTo(Some(List(Tokens.ContinueKeyword("continue"))))
      }

      "default" in {
        keywordsDFA.matchString("default") must beEqualTo(Some(List(Tokens.DefaultKeyword("default"))))
      }

      "do" in {
        keywordsDFA.matchString("do") must beEqualTo(Some(List(Tokens.DoKeyword("do"))))
      }

      "double" in {
        keywordsDFA.matchString("double") must beEqualTo(Some(List(Tokens.DoubleKeyword("double"))))
      }

      "else" in {
        keywordsDFA.matchString("else") must beEqualTo(Some(List(Tokens.ElseKeyword("else"))))
      }

      "extends" in {
        keywordsDFA.matchString("extends") must beEqualTo(Some(List(Tokens.ExtendsKeyword("extends"))))
      }

      "final" in {
        keywordsDFA.matchString("final") must beEqualTo(Some(List(Tokens.FinalKeyword("final"))))
      }

      "finally" in {
        keywordsDFA.matchString("finally") must beEqualTo(Some(List(Tokens.FinallyKeyword("finally"))))
      }

      "float" in {
        keywordsDFA.matchString("float") must beEqualTo(Some(List(Tokens.FloatKeyword("float"))))
      }

      "for" in {
        keywordsDFA.matchString("for") must beEqualTo(Some(List(Tokens.ForKeyword("for"))))
      }

      "goto" in {
        keywordsDFA.matchString("goto") must beEqualTo(Some(List(Tokens.GotoKeyword("goto"))))
      }

      "if" in {
        keywordsDFA.matchString("if") must beEqualTo(Some(List(Tokens.IfKeyword("if"))))
      }

      "implements" in {
        keywordsDFA.matchString("implements") must beEqualTo(Some(List(Tokens.ImplementsKeyword("implements"))))
      }

      "import" in {
        keywordsDFA.matchString("import") must beEqualTo(Some(List(Tokens.ImportKeyword("import"))))
      }

      "instanceof" in {
        keywordsDFA.matchString("instanceof") must beEqualTo(Some(List(Tokens.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        keywordsDFA.matchString("int") must beEqualTo(Some(List(Tokens.IntKeyword("int"))))
      }

      "interface" in {
        keywordsDFA.matchString("interface") must beEqualTo(Some(List(Tokens.InterfaceKeyword("interface"))))
      }

      "long" in {
        keywordsDFA.matchString("long") must beEqualTo(Some(List(Tokens.LongKeyword("long"))))
      }

      "native" in {
        keywordsDFA.matchString("native") must beEqualTo(Some(List(Tokens.NativeKeyword("native"))))
      }

      "new" in {
        keywordsDFA.matchString("new") must beEqualTo(Some(List(Tokens.NewKeyword("new"))))
      }

      "package" in {
        keywordsDFA.matchString("package") must beEqualTo(Some(List(Tokens.PackageKeyword("package"))))
      }

      "private" in {
        keywordsDFA.matchString("private") must beEqualTo(Some(List(Tokens.PrivateKeyword("private"))))
      }

      "protected" in {
        keywordsDFA.matchString("protected") must beEqualTo(Some(List(Tokens.ProtectedKeyword("protected"))))
      }

      "public" in {
        keywordsDFA.matchString("public") must beEqualTo(Some(List(Tokens.PublicKeyword("public"))))
      }

      "return" in {
        keywordsDFA.matchString("return") must beEqualTo(Some(List(Tokens.ReturnKeyword("return"))))
      }

      "short" in {
        keywordsDFA.matchString("short") must beEqualTo(Some(List(Tokens.ShortKeyword("short"))))
      }

      "static" in {
        keywordsDFA.matchString("static") must beEqualTo(Some(List(Tokens.StaticKeyword("static"))))
      }

      "strictfp" in {
        keywordsDFA.matchString("strictfp") must beEqualTo(Some(List(Tokens.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        keywordsDFA.matchString("super") must beEqualTo(Some(List(Tokens.SuperKeyword("super"))))
      }

      "switch" in {
        keywordsDFA.matchString("switch") must beEqualTo(Some(List(Tokens.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        keywordsDFA.matchString("synchronized") must beEqualTo(Some(List(Tokens.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        keywordsDFA.matchString("this") must beEqualTo(Some(List(Tokens.ThisKeyword("this"))))
      }

      "throw" in {
        keywordsDFA.matchString("throw") must beEqualTo(Some(List(Tokens.ThrowKeyword("throw"))))
      }

      "throws" in {
        keywordsDFA.matchString("throws") must beEqualTo(Some(List(Tokens.ThrowsKeyword("throws"))))
      }

      "transient" in {
        keywordsDFA.matchString("transient") must beEqualTo(Some(List(Tokens.TransientKeyword("transient"))))
      }

      "try" in {
        keywordsDFA.matchString("try") must beEqualTo(Some(List(Tokens.TryKeyword("try"))))
      }

      "void" in {
        keywordsDFA.matchString("void") must beEqualTo(Some(List(Tokens.VoidKeyword("void"))))
      }

      "volatile" in {
        keywordsDFA.matchString("volatile") must beEqualTo(Some(List(Tokens.VolatileKeyword("volatile"))))
      }

      "while" in {
        keywordsDFA.matchString("while") must beEqualTo(Some(List(Tokens.WhileKeyword("while"))))
      }


      "true" in {
        keywordsDFA.matchString("true") must beEqualTo(Some(List(Tokens.TrueLiteral("true"))))
      }

      "false" in {
        keywordsDFA.matchString("false") must beEqualTo(Some(List(Tokens.FalseLiteral("false"))))
      }

      "null" in {
        keywordsDFA.matchString("null") must beEqualTo(Some(List(Tokens.NullLiteral("null"))))
      }
    }

    "similar prefixes" in {
      "pathological case" in {
        val keywordsNFA: NFA = NFA.union(Set(
          TokenNFA.nfas(TokenTypes.CaseKeyword),
          TokenNFA.nfas(TokenTypes.CatchKeyword),
          TokenNFA.nfas(TokenTypes.CharKeyword)
        ))
        val keywordsDFA: DFA = keywordsNFA.toDFA

        keywordsDFA.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
        keywordsDFA.matchString("char") must beEqualTo(Some(List(Tokens.CharKeyword("char"))))
        keywordsDFA.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
      }

      "case, catch" in {
        val keywordsNFA: NFA = NFA.union(Set(
          TokenNFA.nfas(TokenTypes.CaseKeyword),
          TokenNFA.nfas(TokenTypes.CatchKeyword),
          TokenNFA.nfas(TokenTypes.Identifier)
        ))
        val keywordsDFA: DFA = keywordsNFA.toDFA

        keywordsDFA.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
        keywordsDFA.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
        keywordsDFA.matchString("cats") must beEqualTo(Some(List(Tokens.Identifier("cats"))))
      }
    }

    "keywords in identifier" in {
      val matchAgainst = NFA.union(Set(
        TokenNFA.nfas(TokenTypes.Identifier),
        TokenNFA.keyword
      )).toDFA

      "abstract" in {
        matchAgainst.matchString("abstract") must beEqualTo(Some(List(Tokens.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        matchAgainst.matchString("boolean") must beEqualTo(Some(List(Tokens.BooleanKeyword("boolean"))))
      }

      "break" in {
        matchAgainst.matchString("break") must beEqualTo(Some(List(Tokens.BreakKeyword("break"))))
      }

      "byte" in {
        matchAgainst.matchString("byte") must beEqualTo(Some(List(Tokens.ByteKeyword("byte"))))
      }

      "case" in {
        matchAgainst.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
      }

      "catch" in {
        matchAgainst.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
      }

      "char" in {
        matchAgainst.matchString("char") must beEqualTo(Some(List(Tokens.CharKeyword("char"))))
      }

      "class" in {
        matchAgainst.matchString("class") must beEqualTo(Some(List(Tokens.ClassKeyword("class"))))
      }

      "const" in {
        matchAgainst.matchString("const") must beEqualTo(Some(List(Tokens.ConstKeyword("const"))))
      }

      "continue" in {
        matchAgainst.matchString("continue") must beEqualTo(Some(List(Tokens.ContinueKeyword("continue"))))
      }

      "default" in {
        matchAgainst.matchString("default") must beEqualTo(Some(List(Tokens.DefaultKeyword("default"))))
      }

      "do" in {
        matchAgainst.matchString("do") must beEqualTo(Some(List(Tokens.DoKeyword("do"))))
      }

      "double" in {
        matchAgainst.matchString("double") must beEqualTo(Some(List(Tokens.DoubleKeyword("double"))))
      }

      "else" in {
        matchAgainst.matchString("else") must beEqualTo(Some(List(Tokens.ElseKeyword("else"))))
      }

      "extends" in {
        matchAgainst.matchString("extends") must beEqualTo(Some(List(Tokens.ExtendsKeyword("extends"))))
      }

      "final" in {
        matchAgainst.matchString("final") must beEqualTo(Some(List(Tokens.FinalKeyword("final"))))
      }

      "finally" in {
        matchAgainst.matchString("finally") must beEqualTo(Some(List(Tokens.FinallyKeyword("finally"))))
      }

      "float" in {
        matchAgainst.matchString("float") must beEqualTo(Some(List(Tokens.FloatKeyword("float"))))
      }

      "for" in {
        matchAgainst.matchString("for") must beEqualTo(Some(List(Tokens.ForKeyword("for"))))
      }

      "goto" in {
        matchAgainst.matchString("goto") must beEqualTo(Some(List(Tokens.GotoKeyword("goto"))))
      }

      "if" in {
        matchAgainst.matchString("if") must beEqualTo(Some(List(Tokens.IfKeyword("if"))))
      }

      "implements" in {
        matchAgainst.matchString("implements") must beEqualTo(Some(List(Tokens.ImplementsKeyword("implements"))))
      }

      "import" in {
        matchAgainst.matchString("import") must beEqualTo(Some(List(Tokens.ImportKeyword("import"))))
      }

      "instanceof" in {
        matchAgainst.matchString("instanceof") must beEqualTo(Some(List(Tokens.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        matchAgainst.matchString("int") must beEqualTo(Some(List(Tokens.IntKeyword("int"))))
      }

      "interface" in {
        matchAgainst.matchString("interface") must beEqualTo(Some(List(Tokens.InterfaceKeyword("interface"))))
      }

      "long" in {
        matchAgainst.matchString("long") must beEqualTo(Some(List(Tokens.LongKeyword("long"))))
      }

      "native" in {
        matchAgainst.matchString("native") must beEqualTo(Some(List(Tokens.NativeKeyword("native"))))
      }

      "new" in {
        matchAgainst.matchString("new") must beEqualTo(Some(List(Tokens.NewKeyword("new"))))
      }

      "package" in {
        matchAgainst.matchString("package") must beEqualTo(Some(List(Tokens.PackageKeyword("package"))))
      }

      "private" in {
        matchAgainst.matchString("private") must beEqualTo(Some(List(Tokens.PrivateKeyword("private"))))
      }

      "protected" in {
        matchAgainst.matchString("protected") must beEqualTo(Some(List(Tokens.ProtectedKeyword("protected"))))
      }

      "public" in {
        matchAgainst.matchString("public") must beEqualTo(Some(List(Tokens.PublicKeyword("public"))))
      }

      "return" in {
        matchAgainst.matchString("return") must beEqualTo(Some(List(Tokens.ReturnKeyword("return"))))
      }

      "short" in {
        matchAgainst.matchString("short") must beEqualTo(Some(List(Tokens.ShortKeyword("short"))))
      }

      "static" in {
        matchAgainst.matchString("static") must beEqualTo(Some(List(Tokens.StaticKeyword("static"))))
      }

      "strictfp" in {
        matchAgainst.matchString("strictfp") must beEqualTo(Some(List(Tokens.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        matchAgainst.matchString("super") must beEqualTo(Some(List(Tokens.SuperKeyword("super"))))
      }

      "switch" in {
        matchAgainst.matchString("switch") must beEqualTo(Some(List(Tokens.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        matchAgainst.matchString("synchronized") must beEqualTo(Some(List(Tokens.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        matchAgainst.matchString("this") must beEqualTo(Some(List(Tokens.ThisKeyword("this"))))
      }

      "throw" in {
        matchAgainst.matchString("throw") must beEqualTo(Some(List(Tokens.ThrowKeyword("throw"))))
      }

      "throws" in {
        matchAgainst.matchString("throws") must beEqualTo(Some(List(Tokens.ThrowsKeyword("throws"))))
      }

      "transient" in {
        val nfa = TokenNFA.nfas(TokenTypes.Identifier)
        matchAgainst.matchString("transient") must beEqualTo(Some(List(Tokens.TransientKeyword("transient"))))
      }

      "try" in {
        matchAgainst.matchString("try") must beEqualTo(Some(List(Tokens.TryKeyword("try"))))
      }

      "void" in {
        matchAgainst.matchString("void") must beEqualTo(Some(List(Tokens.VoidKeyword("void"))))
      }

      "volatile" in {
        matchAgainst.matchString("volatile") must beEqualTo(Some(List(Tokens.VolatileKeyword("volatile"))))
      }

      "while" in {
        matchAgainst.matchString("while") must beEqualTo(Some(List(Tokens.WhileKeyword("while"))))
      }


      "true" in {
        matchAgainst.matchString("true") must beEqualTo(Some(List(TokenTypes.TrueLiteral.create("true"))))
      }

      "false" in {
        matchAgainst.matchString("false") must beEqualTo(Some(List(TokenTypes.FalseLiteral.create("false"))))
      }

      "null" in {
        matchAgainst.matchString("null") must beEqualTo(Some(List(TokenTypes.NullLiteral.create("null"))))
      }
    }

    "keywords in total NFA" in {
      val totalDFA: DFA = TokenNFA.nfa.toDFA

      "abstract" in {
        totalDFA.matchString("abstract") must beEqualTo(Some(List(Tokens.AbstractKeyword("abstract"))))
      }

      "boolean" in {
        totalDFA.matchString("boolean") must beEqualTo(Some(List(Tokens.BooleanKeyword("boolean"))))
      }

      "break" in {
        totalDFA.matchString("break") must beEqualTo(Some(List(Tokens.BreakKeyword("break"))))
      }

      "byte" in {
        totalDFA.matchString("byte") must beEqualTo(Some(List(Tokens.ByteKeyword("byte"))))
      }

      "case" in {
        totalDFA.matchString("case") must beEqualTo(Some(List(Tokens.CaseKeyword("case"))))
      }

      "catch" in {
        totalDFA.matchString("catch") must beEqualTo(Some(List(Tokens.CatchKeyword("catch"))))
      }

      "char" in {
        totalDFA.matchString("char") must beEqualTo(Some(List(Tokens.CharKeyword("char"))))
      }

      "class" in {
        totalDFA.matchString("class") must beEqualTo(Some(List(Tokens.ClassKeyword("class"))))
      }

      "const" in {
        totalDFA.matchString("const") must beEqualTo(Some(List(Tokens.ConstKeyword("const"))))
      }

      "continue" in {
        totalDFA.matchString("continue") must beEqualTo(Some(List(Tokens.ContinueKeyword("continue"))))
      }

      "default" in {
        totalDFA.matchString("default") must beEqualTo(Some(List(Tokens.DefaultKeyword("default"))))
      }

      "do" in {
        totalDFA.matchString("do") must beEqualTo(Some(List(Tokens.DoKeyword("do"))))
      }

      "double" in {
        totalDFA.matchString("double") must beEqualTo(Some(List(Tokens.DoubleKeyword("double"))))
      }

      "else" in {
        totalDFA.matchString("else") must beEqualTo(Some(List(Tokens.ElseKeyword("else"))))
      }

      "extends" in {
        totalDFA.matchString("extends") must beEqualTo(Some(List(Tokens.ExtendsKeyword("extends"))))
      }

      "final" in {
        totalDFA.matchString("final") must beEqualTo(Some(List(Tokens.FinalKeyword("final"))))
      }

      "finally" in {
        totalDFA.matchString("finally") must beEqualTo(Some(List(Tokens.FinallyKeyword("finally"))))
      }

      "float" in {
        totalDFA.matchString("float") must beEqualTo(Some(List(Tokens.FloatKeyword("float"))))
      }

      "for" in {
        totalDFA.matchString("for") must beEqualTo(Some(List(Tokens.ForKeyword("for"))))
      }

      "goto" in {
        totalDFA.matchString("goto") must beEqualTo(Some(List(Tokens.GotoKeyword("goto"))))
      }

      "if" in {
        totalDFA.matchString("if") must beEqualTo(Some(List(Tokens.IfKeyword("if"))))
      }

      "implements" in {
        totalDFA.matchString("implements") must beEqualTo(Some(List(Tokens.ImplementsKeyword("implements"))))
      }

      "import" in {
        totalDFA.matchString("import") must beEqualTo(Some(List(Tokens.ImportKeyword("import"))))
      }

      "instanceof" in {
        totalDFA.matchString("instanceof") must beEqualTo(Some(List(Tokens.InstanceofKeyword("instanceof"))))
      }

      "int" in {
        totalDFA.matchString("int") must beEqualTo(Some(List(Tokens.IntKeyword("int"))))
      }

      "interface" in {
        totalDFA.matchString("interface") must beEqualTo(Some(List(Tokens.InterfaceKeyword("interface"))))
      }

      "long" in {
        totalDFA.matchString("long") must beEqualTo(Some(List(Tokens.LongKeyword("long"))))
      }

      "native" in {
        totalDFA.matchString("native") must beEqualTo(Some(List(Tokens.NativeKeyword("native"))))
      }

      "new" in {
        totalDFA.matchString("new") must beEqualTo(Some(List(Tokens.NewKeyword("new"))))
      }

      "package" in {
        totalDFA.matchString("package") must beEqualTo(Some(List(Tokens.PackageKeyword("package"))))
      }

      "private" in {
        totalDFA.matchString("private") must beEqualTo(Some(List(Tokens.PrivateKeyword("private"))))
      }

      "protected" in {
        totalDFA.matchString("protected") must beEqualTo(Some(List(Tokens.ProtectedKeyword("protected"))))
      }

      "public" in {
        totalDFA.matchString("public") must beEqualTo(Some(List(Tokens.PublicKeyword("public"))))
      }

      "return" in {
        totalDFA.matchString("return") must beEqualTo(Some(List(Tokens.ReturnKeyword("return"))))
      }

      "short" in {
        totalDFA.matchString("short") must beEqualTo(Some(List(Tokens.ShortKeyword("short"))))
      }

      "static" in {
        totalDFA.matchString("static") must beEqualTo(Some(List(Tokens.StaticKeyword("static"))))
      }

      "strictfp" in {
        totalDFA.matchString("strictfp") must beEqualTo(Some(List(Tokens.StrictfpKeyword("strictfp"))))
      }

      "super" in {
        totalDFA.matchString("super") must beEqualTo(Some(List(Tokens.SuperKeyword("super"))))
      }

      "switch" in {
        totalDFA.matchString("switch") must beEqualTo(Some(List(Tokens.SwitchKeyword("switch"))))
      }

      "synchronized" in {
        totalDFA.matchString("synchronized") must beEqualTo(Some(List(Tokens.SynchronizedKeyword("synchronized"))))
      }

      "this" in {
        totalDFA.matchString("this") must beEqualTo(Some(List(Tokens.ThisKeyword("this"))))
      }

      "throw" in {
        totalDFA.matchString("throw") must beEqualTo(Some(List(Tokens.ThrowKeyword("throw"))))
      }

      "throws" in {
        totalDFA.matchString("throws") must beEqualTo(Some(List(Tokens.ThrowsKeyword("throws"))))
      }

      "transient" in {
        totalDFA.matchString("transient") must beEqualTo(Some(List(Tokens.TransientKeyword("transient"))))
      }

      "try" in {
        totalDFA.matchString("try") must beEqualTo(Some(List(Tokens.TryKeyword("try"))))
      }

      "void" in {
        totalDFA.matchString("void") must beEqualTo(Some(List(Tokens.VoidKeyword("void"))))
      }

      "volatile" in {
        totalDFA.matchString("volatile") must beEqualTo(Some(List(Tokens.VolatileKeyword("volatile"))))
      }

      "while" in {
        totalDFA.matchString("while") must beEqualTo(Some(List(Tokens.WhileKeyword("while"))))
      }

      "true" in {
        totalDFA.matchString("true") must beEqualTo(Some(List(Tokens.TrueLiteral("true"))))
      }

      "false" in {
        totalDFA.matchString("false") must beEqualTo(Some(List(Tokens.FalseLiteral("false"))))
      }

      "null" in {
        totalDFA.matchString("null") must beEqualTo(Some(List(Tokens.NullLiteral("null"))))
      }
    }

    "matching the entire grammar all at once" in {
      val theDFA = TokenNFA.nfa.toDFA
      theDFA must haveClass[DFA]

      "assign" in {
        theDFA.matchString("=") must beEqualTo(Some(List(Tokens.Assign("="))))
      }

      "equals" in {
        theDFA.matchString("==") must beEqualTo(Some(List(Tokens.Equal("=="))))
      }

      "logical not" in {
        theDFA.matchString("!") must beEqualTo(Some(List(Tokens.LogicalNot("!"))))
      }

      "not equal" in {
        theDFA.matchString("!=") must beEqualTo(Some(List(Tokens.NotEqual("!="))))
      }

      "variable inequality" in {
        theDFA.matchString("a != b") must beEqualTo(Some(List(
          Tokens.Identifier("a"),
          Tokens.Whitespace((" ", 1)),
          Tokens.NotEqual(("!=", 2)),
          Tokens.Whitespace((" ", 4)),
          Tokens.Identifier(("b", 5))
        )))
      }

      "variable equality and assignment" in {
        theDFA.matchString("boolean areEqual = (a == b);") must beEqualTo(Some(List(
          Tokens.BooleanKeyword("boolean"),
          Tokens.Whitespace((" ", 7)),
          Tokens.Identifier(("areEqual", 8)),
          Tokens.Whitespace((" ", 16)),
          Tokens.Assign(("=", 17)),
          Tokens.Whitespace((" ", 18)),
          Tokens.LeftParen(("(", 19)),
          Tokens.Identifier(("a", 20)),
          Tokens.Whitespace((" ", 21)),
          Tokens.Equal(("==", 22)),
          Tokens.Whitespace((" ", 24)),
          Tokens.Identifier(("b", 25)),
          Tokens.RightParen((")", 26)),
          Tokens.Semicolon((";", 27))
        )))
      }

      "class declaration" in {
        theDFA.matchString("""public static Bicycle(int startGear) {
  gear = startGear;
}""") must beEqualTo(Some(List(
          Tokens.PublicKeyword("public"),
          Tokens.Whitespace((" ", 6)),
          Tokens.StaticKeyword(("static", 7)),
          Tokens.Whitespace((" ", 13)),
          Tokens.Identifier(("Bicycle", 14)),
          Tokens.LeftParen(("(", 21)),
          Tokens.IntKeyword(("int", 22)),
          Tokens.Whitespace((" ", 25)),
          Tokens.Identifier(("startGear", 26)),
          Tokens.RightParen((")", 35)),
          Tokens.Whitespace((" ", 36)),
          Tokens.LeftCurly(("{", 37)),
          Tokens.Whitespace(("\n  ", 38)),
          Tokens.Identifier(("gear", 2, 2)),
          Tokens.Whitespace((" ", 2, 6)),
          Tokens.Assign(("=", 2, 7)),
          Tokens.Whitespace((" ", 2, 8)),
          Tokens.Identifier(("startGear", 2, 9)),
          Tokens.Semicolon((";", 2, 18)),
          Tokens.Whitespace(("\n", 2, 19)),
          Tokens.RightCurly(("}", 3, 0))
        )))
      }
    }
  }
}
