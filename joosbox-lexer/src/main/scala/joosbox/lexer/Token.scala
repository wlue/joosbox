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
  object GreaterThan extends Token
  object GreaterEqual extends Token
  object ShiftRight extends Token
  object ShiftRightAssign extends Token
  object BinaryShiftRight extends Token
  object BinaryShiftRightAssign extends Token
  object LessThan extends Token
  object LessEqual extends Token
  object ShiftLeft extends Token
  object ShiftLeftAssign extends Token
  object BinaryXor extends Token
  object BinaryXorAssign extends Token
  object BinaryOr extends Token
  object BinaryOrAssign extends Token
  object LogicalOr extends Token
  object BinaryAnd extends Token
  object BinaryAndAssign extends Token
  object LogicalAnd extends Token
  object Semicolon extends Token

  object Whitespace extends Token
  object SingleLineComment extends Token
  object MultiLineComment extends Token
  object JavaDocComment extends Token

  object Num extends Token
  object Identifier extends Token {
    val Keywords : Map[String, Token.Token] = Map(
      "abstract" -> Token.AbstractKeyword,
      "boolean" -> Token.BooleanKeyword,
      "break" -> Token.BreakKeyword,
      "byte" -> Token.ByteKeyword,
      "case" -> Token.CaseKeyword,
      "catch" -> Token.CatchKeyword,
      "char" -> Token.CharKeyword,
      "class" -> Token.ClassKeyword,
      "const" -> Token.ConstKeyword,
      "continue" -> Token.ContinueKeyword,
      "default" -> Token.DefaultKeyword,
      "do" -> Token.DoKeyword,
      "double" -> Token.DoubleKeyword,
      "else" -> Token.ElseKeyword,
      "extends" -> Token.ExtendsKeyword,
      "final" -> Token.FinalKeyword,
      "finally" -> Token.FinallyKeyword,
      "float" -> Token.FloatKeyword,
      "for" -> Token.ForKeyword,
      "goto" -> Token.GotoKeyword,
      "if" -> Token.IfKeyword,
      "implements" -> Token.ImplementsKeyword,
      "import" -> Token.ImportKeyword,
      "instanceof" -> Token.InstanceofKeyword,
      "int" -> Token.IntKeyword,
      "interface" -> Token.InterfaceKeyword,
      "long" -> Token.LongKeyword,
      "native" -> Token.NativeKeyword,
      "new" -> Token.NewKeyword,
      "package" -> Token.PackageKeyword,
      "private" -> Token.PrivateKeyword,
      "protected" -> Token.ProtectedKeyword,
      "public" -> Token.PublicKeyword,
      "return" -> Token.ReturnKeyword,
      "short" -> Token.ShortKeyword,
      "static" -> Token.StaticKeyword,
      "strictfp" -> Token.StrictfpKeyword,
      "super" -> Token.SuperKeyword,
      "switch" -> Token.SwitchKeyword,
      "synchronized" -> Token.SynchronizedKeyword,
      "this" -> Token.ThisKeyword,
      "throw" -> Token.ThrowKeyword,
      "throws" -> Token.ThrowsKeyword,
      "transient" -> Token.TransientKeyword,
      "try" -> Token.TryKeyword,
      "void" -> Token.VoidKeyword,
      "volatile" -> Token.VolatileKeyword,
      "while" -> Token.WhileKeyword,

      "true" -> Token.TrueLiteral,
      "false" -> Token.FalseLiteral,
      "null" -> Token.NullLiteral
    )
  }

  object CharLiteral extends Token
  object StringLiteral extends Token

	object AbstractKeyword extends Token
	object BooleanKeyword extends Token
	object BreakKeyword extends Token
	object ByteKeyword extends Token
	object CaseKeyword extends Token
	object CatchKeyword extends Token
	object CharKeyword extends Token
	object ClassKeyword extends Token
	object ConstKeyword extends Token
	object ContinueKeyword extends Token
	object DefaultKeyword extends Token
	object DoKeyword extends Token
	object DoubleKeyword extends Token
	object ElseKeyword extends Token
	object ExtendsKeyword extends Token
	object FinalKeyword extends Token
	object FinallyKeyword extends Token
	object FloatKeyword extends Token
	object ForKeyword extends Token
	object GotoKeyword extends Token
	object IfKeyword extends Token
	object ImplementsKeyword extends Token
	object ImportKeyword extends Token
	object InstanceofKeyword extends Token
	object IntKeyword extends Token
	object InterfaceKeyword extends Token
	object LongKeyword extends Token
	object NativeKeyword extends Token
	object NewKeyword extends Token
	object PackageKeyword extends Token
	object PrivateKeyword extends Token
	object ProtectedKeyword extends Token
	object PublicKeyword extends Token
	object ReturnKeyword extends Token
	object ShortKeyword extends Token
	object StaticKeyword extends Token
	object StrictfpKeyword extends Token
	object SuperKeyword extends Token
	object SwitchKeyword extends Token
	object SynchronizedKeyword extends Token
	object ThisKeyword extends Token
	object ThrowKeyword extends Token
	object ThrowsKeyword extends Token
	object TransientKeyword extends Token
	object TryKeyword extends Token
	object VoidKeyword extends Token
	object VolatileKeyword extends Token
	object WhileKeyword extends Token

  object TrueLiteral extends Token
  object FalseLiteral extends Token
  object NullLiteral extends Token
}

object TokenNFA {
  lazy val nfa: NFA = NFA.union(nfas.values.toSet)

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
    ),

    Token.BinaryNot -> NFA(
      Set(State("i"), State("~")),
      Set(Symbol.epsilon, Symbol("~")),
      Relation(Map(State("i") -> Map(Symbol("~") -> Set(State("~"))))),
      State("i"),
      Set(State("~")),
      Some("BNOT")
    ),

    Token.NotEqual -> NFA(
      Set(State("i"), State("!"), State("!=")),
      Set(Symbol.epsilon, Symbol("!"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))),
                   State("!") -> Map(Symbol("=") -> Set(State("!=")))
               )),
      State("i"),
      Set(State("!=")),
      Some("NOT_EQUAL")
    ),

    Token.Divide -> NFA(
      Set(State("i"), State("/")),
      Set(Symbol.epsilon, Symbol("/")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))))),
      State("i"),
      Set(State("/")),
      Some("DIV")
    ),

    Token.DivideAssign -> NFA(
      Set(State("i"), State("/"), State("/=")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))),
                   State("/") -> Map(Symbol("=") -> Set(State("/=")))
               )),
      State("i"),
      Set(State("/=")),
      Some("DIV_ASSIGN")
    ),

    Token.Plus -> NFA(
      Set(State("i"), State("+")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))))),
      State("i"),
      Set(State("+")),
      Some("PLUS")
    ),

    Token.PlusAssign -> NFA(
      Set(State("i"), State("+"), State("+=")),
      Set(Symbol.epsilon, Symbol("+"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("=") -> Set(State("+=")))
               )),
      State("i"),
      Set(State("+=")),
      Some("PLUS_ASSIGN")
    ),

    Token.Increment -> NFA(
      Set(State("i"), State("+"), State("++")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("+") -> Set(State("++")))
               )),
      State("i"),
      Set(State("++")),
      Some("INC")
    ),

    Token.Minus -> NFA(
      Set(State("i"), State("-")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))))),
      State("i"),
      Set(State("-")),
      Some("MINUS")
    ),

    Token.MinusAssign -> NFA(
      Set(State("i"), State("-"), State("-=")),
      Set(Symbol.epsilon, Symbol("-"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("=") -> Set(State("-=")))
               )),
      State("i"),
      Set(State("-=")),
      Some("MINUS_ASSIGN")
    ),

    Token.Decrement -> NFA(
      Set(State("i"), State("-"), State("--")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("-") -> Set(State("--")))
               )),
      State("i"),
      Set(State("--")),
      Some("DEC")
    ),

    Token.Star -> NFA(
      Set(State("i"), State("*")),
      Set(Symbol.epsilon, Symbol("*")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))))),
      State("i"),
      Set(State("*")),
      Some("STAR")
    ),

    Token.StarAssign -> NFA(
      Set(State("i"), State("*"), State("*=")),
      Set(Symbol.epsilon, Symbol("*"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))),
                   State("*") -> Map(Symbol("=") -> Set(State("*=")))
               )),
      State("i"),
      Set(State("*=")),
      Some("STAR_ASSIGN")
    ),

    Token.Modulo -> NFA(
      Set(State("i"), State("%")),
      Set(Symbol.epsilon, Symbol("%")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))))),
      State("i"),
      Set(State("%")),
      Some("MOD")
    ),

    Token.ModuloAssign -> NFA(
      Set(State("i"), State("%"), State("%=")),
      Set(Symbol.epsilon, Symbol("%"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))),
                   State("%") -> Map(Symbol("=") -> Set(State("%=")))
               )),
      State("i"),
      Set(State("%=")),
      Some("MOD_ASSIGN")
    ),

    Token.GreaterThan -> NFA(
      Set(State("i"), State(">")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))))),
      State("i"),
      Set(State(">")),
      Some("GT")
    ),

    Token.GreaterEqual -> NFA(
      Set(State("i"), State(">"), State(">=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol("=") -> Set(State(">=")))
               )),
      State("i"),
      Set(State(">=")),
      Some("GTE")
    ),

    Token.ShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol(">") -> Set(State(">>")))
               )),
      State("i"),
      Set(State(">>")),
      Some("SR")
    ),

    Token.ShiftRightAssign -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol("=") -> Set(State(">>=")))
               )),
      State("i"),
      Set(State(">>=")),
      Some("SR_ASSIGN")
    ),

    Token.BinaryShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol(">") -> Set(State(">>>")))
               )),
      State("i"),
      Set(State(">>>")),
      Some("BSR")
    ),

    Token.BinaryShiftRightAssign -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>>"), State(">>>=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol(">") -> Set(State(">>>"))),
                   State(">>>") -> Map(Symbol("=") -> Set(State(">>>=")))
               )),
      State("i"),
      Set(State(">>>=")),
      Some("BSR_ASSIGN")
    ),

    Token.LessThan -> NFA(
      Set(State("i"), State("<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))))),
      State("i"),
      Set(State("<")),
      Some("LT")
    ),

    Token.LessEqual -> NFA(
      Set(State("i"), State("<"), State("<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("=") -> Set(State("<=")))
               )),
      State("i"),
      Set(State("<=")),
      Some("LE")
    ),

    Token.ShiftLeft -> NFA(
      Set(State("i"), State("<"), State("<<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("<") -> Set(State("<<")))
               )),
      State("i"),
      Set(State("<<")),
      Some("SL")
    ),

    Token.ShiftLeftAssign -> NFA(
      Set(State("i"), State("<"), State("<<"), State("<<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol("<") -> Set(State("<"))),
                   State("<")   -> Map(Symbol("<") -> Set(State("<<"))),
                   State("<<")  -> Map(Symbol("=") -> Set(State("<<=")))
               )),
      State("i"),
      Set(State("<<=")),
      Some("SL_ASSIGN")
    ),

    Token.BinaryXor -> NFA(
      Set(State("i"), State("^")),
      Set(Symbol.epsilon, Symbol("^")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))))),
      State("i"),
      Set(State("^")),
      Some("BXOR")
    ),

    Token.BinaryXorAssign -> NFA(
      Set(State("i"), State("^"), State("^=")),
      Set(Symbol.epsilon, Symbol("^"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))),
                   State("^") -> Map(Symbol("=") -> Set(State("^=")))
               )),
      State("i"),
      Set(State("^=")),
      Some("BXOR_ASSIGN")
    ),

  Token.BinaryOr -> NFA(
      Set(State("i"), State("|")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))))),
      State("i"),
      Set(State("|")),
      Some("BOR")
    ),

    Token.BinaryOrAssign -> NFA(
      Set(State("i"), State("|"), State("|=")),
      Set(Symbol.epsilon, Symbol("|"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("=") -> Set(State("|=")))
               )),
      State("i"),
      Set(State("|=")),
      Some("BOR_ASSIGN")
    ),

    Token.LogicalOr -> NFA(
      Set(State("i"), State("|"), State("||")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("|") -> Set(State("||")))
               )),
      State("i"),
      Set(State("||")),
      Some("LOR")
    ),

    Token.BinaryAnd -> NFA(
      Set(State("i"), State("&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))))),
      State("i"),
      Set(State("&")),
      Some("BAND")
    ),

    Token.BinaryAndAssign -> NFA(
      Set(State("i"), State("&"), State("&=")),
      Set(Symbol.epsilon, Symbol("&"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("=") -> Set(State("&=")))
               )),
      State("i"),
      Set(State("&=")),
      Some("BAND_ASSIGN")
    ),

    Token.LogicalAnd -> NFA(
      Set(State("i"), State("&"), State("&&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("&") -> Set(State("&&")))
               )),
      State("i"),
      Set(State("&&")),
      Some("LAND")
    ),

    Token.Semicolon -> NFA(
      Set(State("i"), State(";")),
      Set(Symbol.epsilon, Symbol(";")),
      Relation(Map(State("i") -> Map(Symbol(";") -> Set(State(";"))))),
      State("i"),
      Set(State(";")),
      Some("SEMI")
    ),

    Token.Whitespace -> NFA(
      Set(State("i"), State("ws")),
      Set(Symbol.epsilon, Symbol(" "), Symbol("\n"), Symbol("\r"),
          Symbol("\t"), Symbol("\f"), Symbol("\13")),
      Relation(
        Map(
          State("i") -> Map(
            Symbol(" ") -> Set(State("ws")),
            Symbol("\n") -> Set(State("ws")),
            Symbol("\r") -> Set(State("ws")),
            Symbol("\t") -> Set(State("ws")),
            Symbol("\f") -> Set(State("ws")),
            Symbol("\13") -> Set(State("ws"))
          ),
          State("ws") -> Map(
            Symbol(" ") -> Set(State("ws")),
            Symbol("\n") -> Set(State("ws")),
            Symbol("\r") -> Set(State("ws")),
            Symbol("\t") -> Set(State("ws")),
            Symbol("\f") -> Set(State("ws")),
            Symbol("\13") -> Set(State("ws"))
          )
        )
      ),
      State("i"),
      Set(State("ws")),
      Some("WHITESPACE")
    ),

    Token.SingleLineComment -> NFA(
      Set(State("i"), State("/"), State("//"), State("eol"), State("eol2")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("\n"), Symbol("\r"), NegatedSymbols("\n", "\r")),
      Relation(Map(State("i")     -> Map( Symbol("/") -> Set(State("/"))),
                   State("/")     -> Map( Symbol("/") -> Set(State("//"))),
                   State("//")    -> Map( Symbol("\n") -> Set(State("eol")),
                                          Symbol("\r") -> Set(State("eol2")),
                                          NegatedSymbols("\n", "\r") -> Set(State("//"))),
                   State("eol2")  -> Map( Symbol("\n") -> Set(State("eol")))
              )),
      State("i"),
      Set(State("eol"), State("eol2")),
      Some("SL_COMMENT")
    ),

    Token.MultiLineComment -> NFA(
      Set(State("i"), State("/"), State("/*"), State("/**"), State("/**/")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("*"), NegatedSymbols("*"), NegatedSymbols("*", "/")),
      Relation(Map(State("i")   -> Map( Symbol("/") -> Set(State("/"))),
                   State("/")   -> Map( Symbol("*") -> Set(State("/*"))),
                   State("/*")  -> Map( Symbol("*") -> Set(State("/**")),
                                        NegatedSymbols("*") -> Set(State("/*"))),
                   State("/**") -> Map( Symbol("/") -> Set(State("/**/")),
                                        Symbol("*") -> Set(State("/**")),
                                        NegatedSymbols("*", "/") -> Set(State("/*")))
              )),
      State("i"),
      Set(State("/**/")),
      Some("ML_COMMENT")
    ),

    Token.JavaDocComment -> NFA(
      Set(State("i"), State("/"), State("/*"), State("/**"), State("/***"), State("/***/")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("*"), NegatedSymbols("*"), NegatedSymbols("*", "/")),
      Relation(Map(State("i")     -> Map( Symbol("/") -> Set(State("/"))),
                   State("/")     -> Map( Symbol("*") -> Set(State("/*"))),
                   State("/*")    -> Map( Symbol("*") -> Set(State("/**"))),
                   State("/**")   -> Map( Symbol("*") -> Set(State("/***")),
                                          NegatedSymbols("*") -> Set(State("/**"))),
                   State("/***")  -> Map( Symbol("/") -> Set(State("/***/")),
                                          Symbol("*") -> Set(State("/***")),
                                          NegatedSymbols("*", "/") -> Set(State("/**")))
              )),
      State("i"),
      Set(State("/***/")),
      Some("JD_COMMENT")
    ),

    Token.Num -> NFA(
      Set(State("i"), State("digit")),
      Set(Symbol.epsilon, Symbol.digitsGroup),
      Relation(Map(State("i") -> Map(Symbol.digitsGroup -> Set(State("digit"))),
                   State("digit") -> Map(Symbol.digitsGroup -> Set(State("digit")))
               )),
      State("i"),
      Set(State("digit")),
      Some("NUM")
    ),

    Token.Identifier -> NFA(
      Set(State("i"), State("id")),
      Set(Symbol.epsilon, Symbol("_"), Symbol("$"), Symbol.lettersGroup, Symbol.digitsGroup),
      Relation(Map(State("i")   -> Map( Symbol.lettersGroup -> Set(State("id")),
                                        Symbol("_") -> Set(State("id")),
                                        Symbol("$") -> Set(State("id"))),
                   State("id")  -> Map( Symbol.lettersGroup -> Set(State("id")),
                                        Symbol.digitsGroup -> Set(State("id")),
                                        Symbol("_") -> Set(State("id")),
                                        Symbol("$") -> Set(State("id")))
               )),
      State("i"),
      Set(State("id")),
      Some("IDENTIFIER")
    ),

    Token.CharLiteral -> NFA(
      Set(State("i"), State("\'"), State("\'\\"), State("char-part"), State("char"),
          State("oct"), State("oct2"),
          State("part-oct"), State("part-oct2"), State("part-oct3")),
      Set(Symbol("\\"), Symbol("n"), Symbol("r"), Symbol("t"), Symbol("b"),
          Symbol("f"), Symbol("\""), Symbol("\'"), NegatedSymbols("\\", "\n", "\r"),
          Symbol.octalDigitsGroup, Symbol.quadDigitsGroup),
      Relation(Map(State("i")     -> Map( Symbol("\'") -> Set(State("\'"))),
                   State("\'")    -> Map( Symbol("\\") -> Set(State("\'\\")),
                                          Symbol("\'") -> Set(State("char")),
                                          NegatedSymbols("\\", "\n", "\r") -> Set(State("char-part"))),
                   State("\'\\")  -> Map( Symbol("n") -> Set(State("char-part")),
                                          Symbol("r") -> Set(State("char-part")),
                                          Symbol("t") -> Set(State("char-part")),
                                          Symbol("b") -> Set(State("char-part")),
                                          Symbol("f") -> Set(State("char-part")),
                                          Symbol("\"") -> Set(State("char-part")),
                                          Symbol("\'") -> Set(State("char-part")),
                                          Symbol.octalDigitsGroup -> Set(State("oct")),
                                          Symbol.quadDigitsGroup -> Set(State("part-oct"))),
                   State("char-part") -> Map( Symbol("\'") -> Set(State("char"))),
                   State("oct")       -> Map( Symbol("\'") -> Set(State("char")),
                                              Symbol.octalDigitsGroup -> Set(State("oct2"))),
                   State("oct2")      -> Map( Symbol("\'") -> Set(State("char"))),
                   State("part-oct")  -> Map( Symbol("\'") -> Set(State("char")),
                                              Symbol.octalDigitsGroup -> Set(State("part-oct2"))),
                   State("part-oct2") -> Map( Symbol("\'") -> Set(State("char")),
                                              Symbol.octalDigitsGroup -> Set(State("part-oct3"))),
                   State("part-oct3") -> Map( Symbol("\'") -> Set(State("char")))
               )),
      State("i"),
      Set(State("char")),
      Some("CHAR_LITERAL")
    ),

    Token.StringLiteral -> NFA(
      Set(State("i"), State("\""), State("\"\\"), State("string")),
      Set(Symbol("\""), Symbol("\\"), NegatedSymbols("\"", "\\", "\n", "\r"),
        NegatedSymbols("\n", "\r")),
      Relation(Map(State("i")     -> Map( Symbol("\"") -> Set(State("\""))),
                   State("\"")    -> Map( Symbol("\"") -> Set(State("string")),
                                          Symbol("\\") -> Set(State("\"\\")),
                                          NegatedSymbols("\"", "\\", "\n", "\r") -> Set(State("\""))),
                   State("\"\\")  -> Map( NegatedSymbols("\n", "\r") -> Set(State("\"")))
               )),
      State("i"),
      Set(State("string")),
      Some("STR_LITERAL")
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
  val GT            = """[>]""".r
  val GE            = """[>][=]""".r
  val SR            = """[>][>]""".r
  val SR_ASSIGN     = """[>][>][=]""".r
  val BSR           = """[>][>][>]""".r
  val BSR_ASSIGN    = """[>][>][>][=]""".r
  val LT            = """[<]""".r
  val LE            = """[<][=]""".r
  val SL            = """[<][<]""".r
  val SL_ASSIGN     = """[<][<][=]""".r
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
  val JD_COMMENT    = """/\*\*([^(\n|\r)]*(\r\n|\r|\n))*\*/""".r

  val NUM           = """\d\d*""".r
  val IDENTIFIER    = """[a-zA-Z_\$][a-zA-Z_0-9\$]*""".r

  val CHAR_LITERAL  = """'(\(\n|\r|\t|\b|\f|\'|\")|^[(\n|\r)])'""".r
  val STR_LITERAL   = new Regex("\"(\\(^[\\n|\\r])|([^(\\n|\\r|\\\\|\")]))*\"")
}
