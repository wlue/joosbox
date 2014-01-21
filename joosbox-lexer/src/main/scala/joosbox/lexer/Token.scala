package joosbox.lexer

import scala.util.matching.Regex

object Token {
  sealed trait Token
  object Question extends Token
  object LeftParen extends Token
  object RightParen extends Token
  object LeftBracket extends Token
  object RightBracket extends Token
  object LeftCurly extends Token
  object RightCurly extends Token
  object Colon extends Token
  object Comma extends Token
  object Dot extends Token
  object Assign extends Token
  object Equal extends Token
  object LogicalNot extends Token
  object BinaryNot extends Token
  object NotEqual extends Token
  object Divide extends Token
  object DivideAssign extends Token
  object Plus extends Token
  object PlusAssign extends Token
  object Increment extends Token
  object Minus extends Token
  object MinusAssign extends Token
  object Decrement extends Token
  object Star extends Token
  object StarAssign extends Token
  object Modulo extends Token
  object ModuloAssign extends Token
  object ShiftRight extends Token
  object ShiftRightAssign extends Token
  object BinaryShiftRight extends Token
  object BinaryShiftRightAssign extends Token
  object GreaterEqual extends Token
  object GreaterThan extends Token
  object ShiftLeft extends Token
  object ShiftLeftAssign extends Token
  object LessEqual extends Token
  object LessThan extends Token
  object BinaryXor extends Token
  object BinaryXorAssign extends Token
  object BinaryOr extends Token
  object BinaryOrAssign extends Token
  object LogicalOr extends Token
  object BinaryAnd extends Token
  object BinaryAndAssign extends Token
  object LogicalAnc extends Token
  object Semicolon extends Token

  object Whitespace extends Token
  object SingleLineComment extends Token
  object MultiLineComment extends Token
  object JavaDocComment extends Token

  object Num extends Token
  object Identifier extends Token

  object EscapeChar extends Token
  object CharLiteral extends Token
  object StringLiteral extends Token
}

object TokenNFA {
  lazy val nfa: NFA = {
    nfas.values.reduce { (first, second) => first.union(second) }
  }

  val nfas: Map[Token.Token, NFA] = Map(
    Token.Question -> NFA(
      Set(State("i"), State("?")),
      Set(Symbol.epsilon, Symbol("?")),
      Relation(Map(State("i") -> Map(Symbol("?") -> Set(State("?"))))),
      State("i"),
      Set(State("?")),
      Some("QUESTION")
    ),

    Token.LeftParen -> NFA(
      Set(State("i"), State("(")),
      Set(Symbol.epsilon, Symbol("(")),
      Relation(Map(State("i") -> Map(Symbol("(") -> Set(State("("))))),
      State("i"),
      Set(State("(")),
      Some("LPAREN")
    ),

    Token.RightParen -> NFA(
      Set(State("i"), State(")")),
      Set(Symbol.epsilon, Symbol(")")),
      Relation(Map(State("i") -> Map(Symbol(")") -> Set(State(")"))))),
      State("i"),
      Set(State(")")),
      Some("RPAREN")
    ),

    Token.LeftBracket -> NFA(
      Set(State("i"), State("[")),
      Set(Symbol.epsilon, Symbol("[")),
      Relation(Map(State("i") -> Map(Symbol("[") -> Set(State("["))))),
      State("i"),
      Set(State("[")),
      Some("LBRACK")
    ),

    Token.RightBracket -> NFA(
      Set(State("i"), State("]")),
      Set(Symbol.epsilon, Symbol("]")),
      Relation(Map(State("i") -> Map(Symbol("]") -> Set(State("]"))))),
      State("i"),
      Set(State("]")),
      Some("RBRACK")
    ),

    Token.LeftCurly -> NFA(
      Set(State("i"), State("{")),
      Set(Symbol.epsilon, Symbol("{")),
      Relation(Map(State("i") -> Map(Symbol("{") -> Set(State("{"))))),
      State("i"),
      Set(State("{")),
      Some("LCURLY")
    ),

    Token.RightCurly -> NFA(
      Set(State("i"), State("}")),
      Set(Symbol.epsilon, Symbol("}")),
      Relation(Map(State("i") -> Map(Symbol("}") -> Set(State("}"))))),
      State("i"),
      Set(State("}")),
      Some("RCURLY")
    ),

    Token.Colon -> NFA(
      Set(State("i"), State(":")),
      Set(Symbol.epsilon, Symbol(":")),
      Relation(Map(State("i") -> Map(Symbol(":") -> Set(State(":"))))),
      State("i"),
      Set(State(":")),
      Some("COLON")
    ),

    Token.Comma -> NFA(
      Set(State("i"), State(",")),
      Set(Symbol.epsilon, Symbol(",")),
      Relation(Map(State("i") -> Map(Symbol(",") -> Set(State(","))))),
      State("i"),
      Set(State(",")),
      Some("COMMA")
    ),

    Token.Dot -> NFA(
      Set(State("i"), State(".")),
      Set(Symbol.epsilon, Symbol(".")),
      Relation(Map(State("i") -> Map(Symbol(".") -> Set(State("."))))),
      State("i"),
      Set(State(".")),
      Some("DOT")
    ),

    Token.Assign -> NFA(
      Set(State("i"), State("=")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))))),
      State("i"),
      Set(State("=")),
      Some("ASSIGN")
    ),

    Token.Equal -> NFA(
      Set(State("i"), State("="), State("==")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))),
                   State("=") -> Map(Symbol("=") -> Set(State("==")))
               )),
      State("i"),
      Set(State("==")),
      Some("EQUAL")
    ),

    Token.LogicalNot -> NFA(
      Set(State("i"), State("!")),
      Set(Symbol.epsilon, Symbol("!")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))))),
      State("i"),
      Set(State("!")),
      Some("LNOT")
    )
  )
}

// Only here for hand-conveting to NFAs
object TokenRegex {
  val QUESTION      = """[?]""".r
  val LPAREN        = """[(]""".r
  val RPAREN        = """[)]""".r
  val LBRACK        = """[\[]""".r
  val RBRACK        = """[\]]""".r
  val LCURLY        = """[{]""".r
  val RCURLY        = """[}]""".r
  val COLON         = """[:]""".r
  val COMMA         = """[,]""".r
  val DOT           = """[.]""".r
  val ASSIGN        = """[=]""".r
  val EQUAL         = """[=][=]""".r
  val LNOT          = """[!]""".r
  val BNOT          = """[~]""".r
  val NOT_EQUAL     = """[!][=]""".r
  val DIV           = """[/]""".r
  val DIV_ASSIGN    = """[/][=]""".r
  val PLUS          = """[+]""".r
  val PLUS_ASSIGN   = """[+][=]""".r
  val INC           = """[+][+]""".r
  val MINUS         = """[-]""".r
  val MINUS_ASSIGN  = """[-][=]""".r
  val DEC           = """[-][-]""".r
  val STAR          = """[*]""".r
  val STAR_ASSIGN   = """[*][=]""".r
  val MOD           = """[%]""".r
  val MOD_ASSIGN    = """[%][=]""".r
  val SR            = """[>][>]""".r
  val SR_ASSIGN     = """[>][>][=]""".r
  val BSR           = """[>][>][>]""".r
  val BSR_ASSIGN    = """[>][>][>][=]""".r
  val GE            = """[>][=]""".r
  val GT            = """[>]""".r
  val SL            = """[<][<]""".r
  val SL_ASSIGN     = """[<][<][=]""".r
  val LE            = """[<][=]""".r
  val LT            = """[<]""".r
  val BXOR          = """[\^]""".r
  val BXOR_ASSIGN   = """[\^][=]""".r
  val BOR           = """[|]""".r
  val BOR_ASSIGN    = """[|][=]""".r
  val LOR           = """[|][|]""".r
  val BAND          = """[&]""".r
  val BAND_ASSIGN   = """[&][=]""".r
  val LAND          = """[&][&]""".r
  val SEMI          = """[;]""".r

  val WHITESPACE    = """\s\s*""".r
  val SL_COMMENT    = """//[^(\n|\r)]*[(\n|\r(\n)?]""".r
  val ML_COMMENT    = """/\*([^(\n|\r)]*(\r\n|\r|\n))*\*/""".r
  val JD_COMMENT    = """/\*\*(\*[^(\n|\r)]*(\r\n|\r|\n))*\*/""".r

  val NUM           = """\d\d*""".r
  val IDENTIFIER    = """[a-zA-Z_\$][a-zA-Z_0-9\$]*""".r

  val ESC_CHAR      = """\\(n|r|t|b|f|"|'|\\|[0..9])""".r
  val CHAR_LITERAL  = new Regex("\'("+ESC_CHAR+"|([^(\\n|\\r|\\\\|\')]))\'")
  val STR_LITERAL   = new Regex("\"("+ESC_CHAR+"|([^(\\n|\\r|\\\\|\")]))*\"")
}
