package joosbox.lexer

import scala.util.matching.Regex

object Token {
  val nfas = Seq(
    NFA(
      Set(State("i"), State("?")),
      Set(Symbol.epsilon, Symbol("?")),
      Relation(Map(State("i") -> Map(Symbol("?") -> Set(State("?"))))),
      State("i"),
      Set(State("?")),
      Some("QUESTION")
    ),

    NFA(
      Set(State("i"), State("(")),
      Set(Symbol.epsilon, Symbol("(")),
      Relation(Map(State("i") -> Map(Symbol("(") -> Set(State("("))))),
      State("i"),
      Set(State("(")),
      Some("LPAREN")
    ),

    NFA(
      Set(State("i"), State(")")),
      Set(Symbol.epsilon, Symbol(")")),
      Relation(Map(State("i") -> Map(Symbol(")") -> Set(State(")"))))),
      State("i"),
      Set(State(")")),
      Some("RPAREN")
    ),

    NFA(
      Set(State("i"), State("[")),
      Set(Symbol.epsilon, Symbol("[")),
      Relation(Map(State("i") -> Map(Symbol("[") -> Set(State("["))))),
      State("i"),
      Set(State("[")),
      Some("LBRACK")
    ),

    NFA(
      Set(State("i"), State("]")),
      Set(Symbol.epsilon, Symbol("]")),
      Relation(Map(State("i") -> Map(Symbol("]") -> Set(State("]"))))),
      State("i"),
      Set(State("]")),
      Some("RBRACK")
    ),

    NFA(
      Set(State("i"), State("{")),
      Set(Symbol.epsilon, Symbol("{")),
      Relation(Map(State("i") -> Map(Symbol("{") -> Set(State("{"))))),
      State("i"),
      Set(State("{")),
      Some("LCURLY")
    ),

    NFA(
      Set(State("i"), State("}")),
      Set(Symbol.epsilon, Symbol("}")),
      Relation(Map(State("i") -> Map(Symbol("}") -> Set(State("}"))))),
      State("i"),
      Set(State("}")),
      Some("RCURLY")
    ),

    NFA(
      Set(State("i"), State(":")),
      Set(Symbol.epsilon, Symbol(":")),
      Relation(Map(State("i") -> Map(Symbol(":") -> Set(State(":"))))),
      State("i"),
      Set(State(":")),
      Some("COLON")
    ),

    NFA(
      Set(State("i"), State(",")),
      Set(Symbol.epsilon, Symbol(",")),
      Relation(Map(State("i") -> Map(Symbol(",") -> Set(State(","))))),
      State("i"),
      Set(State(",")),
      Some("COMMA")
    ),

    NFA(
      Set(State("i"), State(".")),
      Set(Symbol.epsilon, Symbol(".")),
      Relation(Map(State("i") -> Map(Symbol(".") -> Set(State("."))))),
      State("i"),
      Set(State(".")),
      Some("DOT")
    ),

    NFA(
      Set(State("i"), State("=")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))))),
      State("i"),
      Set(State("=")),
      Some("ASSIGN")
    ),

    NFA(
      Set(State("i"), State("="), State("==")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))),
                   State("=") -> Map(Symbol("=") -> Set(State("==")))
               )),
      State("i"),
      Set(State("==")),
      Some("EQUAL")
    ),

    NFA(
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
