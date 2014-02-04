package joosbox.lexer 

import scala.util.matching.Regex

object TokenNFA {
  lazy val nfa: NFA = NFA.union(nfas.values.toSet)

  lazy val nfas = rawNFAs.map {
    case (token: TokenType, nfa: NFA) => token -> nfa.withToken(token)
  }

  lazy val keyword = NFA.union(keywords.values.toSet)

  lazy val keywords = TokenTypes.Keywords.map {
    case (keyword: String, token: TokenType) => token -> NFA.fromString(keyword, token)
  }.toMap

  private val rawNFAs = Map(
    TokenTypes.Question -> NFA(
      Set(State("i"), State("?")),
      Set(Symbol.epsilon, Symbol("?")),
      Relation(Map(State("i") -> Map(Symbol("?") -> Set(State("?"))))),
      State("i"),
      Set(State("?"))
    ),

    TokenTypes.LeftParen -> NFA(
      Set(State("i"), State("(")),
      Set(Symbol.epsilon, Symbol("(")),
      Relation(Map(State("i") -> Map(Symbol("(") -> Set(State("("))))),
      State("i"),
      Set(State("("))
    ),

    TokenTypes.RightParen -> NFA(
      Set(State("i"), State(")")),
      Set(Symbol.epsilon, Symbol(")")),
      Relation(Map(State("i") -> Map(Symbol(")") -> Set(State(")"))))),
      State("i"),
      Set(State(")"))
    ),

    TokenTypes.LeftBracket -> NFA(
      Set(State("i"), State("[")),
      Set(Symbol.epsilon, Symbol("[")),
      Relation(Map(State("i") -> Map(Symbol("[") -> Set(State("["))))),
      State("i"),
      Set(State("["))
    ),

    TokenTypes.RightBracket -> NFA(
      Set(State("i"), State("]")),
      Set(Symbol.epsilon, Symbol("]")),
      Relation(Map(State("i") -> Map(Symbol("]") -> Set(State("]"))))),
      State("i"),
      Set(State("]"))
    ),

    TokenTypes.LeftCurly -> NFA(
      Set(State("i"), State("{")),
      Set(Symbol.epsilon, Symbol("{")),
      Relation(Map(State("i") -> Map(Symbol("{") -> Set(State("{"))))),
      State("i"),
      Set(State("{"))
    ),

    TokenTypes.RightCurly -> NFA(
      Set(State("i"), State("}")),
      Set(Symbol.epsilon, Symbol("}")),
      Relation(Map(State("i") -> Map(Symbol("}") -> Set(State("}"))))),
      State("i"),
      Set(State("}"))
    ),

    TokenTypes.Colon -> NFA(
      Set(State("i"), State(":")),
      Set(Symbol.epsilon, Symbol(":")),
      Relation(Map(State("i") -> Map(Symbol(":") -> Set(State(":"))))),
      State("i"),
      Set(State(":"))
    ),

    TokenTypes.Comma -> NFA(
      Set(State("i"), State(",")),
      Set(Symbol.epsilon, Symbol(",")),
      Relation(Map(State("i") -> Map(Symbol(",") -> Set(State(","))))),
      State("i"),
      Set(State(","))
    ),

    TokenTypes.Dot -> NFA(
      Set(State("i"), State(".")),
      Set(Symbol.epsilon, Symbol(".")),
      Relation(Map(State("i") -> Map(Symbol(".") -> Set(State("."))))),
      State("i"),
      Set(State("."))
    ),

    TokenTypes.Assign -> NFA(
      Set(State("i"), State("=")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))))),
      State("i"),
      Set(State("="))
    ),

    TokenTypes.Equal -> NFA(
      Set(State("i"), State("="), State("==")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))),
                   State("=") -> Map(Symbol("=") -> Set(State("==")))
               )),
      State("i"),
      Set(State("=="))
    ),

    TokenTypes.LogicalNot -> NFA(
      Set(State("i"), State("!")),
      Set(Symbol.epsilon, Symbol("!")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))))),
      State("i"),
      Set(State("!"))
    ),

    TokenTypes.BinaryNot -> NFA(
      Set(State("i"), State("~")),
      Set(Symbol.epsilon, Symbol("~")),
      Relation(Map(State("i") -> Map(Symbol("~") -> Set(State("~"))))),
      State("i"),
      Set(State("~"))
    ),

    TokenTypes.NotEqual -> NFA(
      Set(State("i"), State("!"), State("!=")),
      Set(Symbol.epsilon, Symbol("!"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))),
                   State("!") -> Map(Symbol("=") -> Set(State("!=")))
               )),
      State("i"),
      Set(State("!="))
    ),

    TokenTypes.Divide -> NFA(
      Set(State("i"), State("/")),
      Set(Symbol.epsilon, Symbol("/")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))))),
      State("i"),
      Set(State("/"))
    ),

    TokenTypes.DivideAssign -> NFA(
      Set(State("i"), State("/"), State("/=")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))),
                   State("/") -> Map(Symbol("=") -> Set(State("/=")))
               )),
      State("i"),
      Set(State("/="))
    ),

    TokenTypes.Plus -> NFA(
      Set(State("i"), State("+")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))))),
      State("i"),
      Set(State("+"))
    ),

    TokenTypes.PlusAssign -> NFA(
      Set(State("i"), State("+"), State("+=")),
      Set(Symbol.epsilon, Symbol("+"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("=") -> Set(State("+=")))
               )),
      State("i"),
      Set(State("+="))
    ),

    TokenTypes.Increment -> NFA(
      Set(State("i"), State("+"), State("++")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("+") -> Set(State("++")))
               )),
      State("i"),
      Set(State("++"))
    ),

    TokenTypes.Minus -> NFA(
      Set(State("i"), State("-")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))))),
      State("i"),
      Set(State("-"))
    ),

    TokenTypes.MinusAssign -> NFA(
      Set(State("i"), State("-"), State("-=")),
      Set(Symbol.epsilon, Symbol("-"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("=") -> Set(State("-=")))
               )),
      State("i"),
      Set(State("-="))
    ),

    TokenTypes.Decrement -> NFA(
      Set(State("i"), State("-"), State("--")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("-") -> Set(State("--")))
               )),
      State("i"),
      Set(State("--"))
    ),

    TokenTypes.Star -> NFA(
      Set(State("i"), State("*")),
      Set(Symbol.epsilon, Symbol("*")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))))),
      State("i"),
      Set(State("*"))
    ),

    TokenTypes.StarAssign -> NFA(
      Set(State("i"), State("*"), State("*=")),
      Set(Symbol.epsilon, Symbol("*"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))),
                   State("*") -> Map(Symbol("=") -> Set(State("*=")))
               )),
      State("i"),
      Set(State("*="))
    ),

    TokenTypes.Modulo -> NFA(
      Set(State("i"), State("%")),
      Set(Symbol.epsilon, Symbol("%")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))))),
      State("i"),
      Set(State("%"))
    ),

    TokenTypes.ModuloAssign -> NFA(
      Set(State("i"), State("%"), State("%=")),
      Set(Symbol.epsilon, Symbol("%"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))),
                   State("%") -> Map(Symbol("=") -> Set(State("%=")))
               )),
      State("i"),
      Set(State("%="))
    ),

    TokenTypes.GreaterThan -> NFA(
      Set(State("i"), State(">")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))))),
      State("i"),
      Set(State(">"))
    ),

    TokenTypes.GreaterEqual -> NFA(
      Set(State("i"), State(">"), State(">=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol("=") -> Set(State(">=")))
               )),
      State("i"),
      Set(State(">="))
    ),

    TokenTypes.ShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol(">") -> Set(State(">>")))
               )),
      State("i"),
      Set(State(">>"))
    ),

    TokenTypes.ShiftRightAssign -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol("=") -> Set(State(">>=")))
               )),
      State("i"),
      Set(State(">>="))
    ),

    TokenTypes.BinaryShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol(">") -> Set(State(">>>")))
               )),
      State("i"),
      Set(State(">>>"))
    ),

    TokenTypes.BinaryShiftRightAssign -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>>"), State(">>>=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol(">") -> Set(State(">>>"))),
                   State(">>>") -> Map(Symbol("=") -> Set(State(">>>=")))
               )),
      State("i"),
      Set(State(">>>="))
    ),

    TokenTypes.LessThan -> NFA(
      Set(State("i"), State("<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))))),
      State("i"),
      Set(State("<"))
    ),

    TokenTypes.LessEqual -> NFA(
      Set(State("i"), State("<"), State("<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("=") -> Set(State("<=")))
               )),
      State("i"),
      Set(State("<="))
    ),

    TokenTypes.ShiftLeft -> NFA(
      Set(State("i"), State("<"), State("<<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("<") -> Set(State("<<")))
               )),
      State("i"),
      Set(State("<<"))
    ),

    TokenTypes.ShiftLeftAssign -> NFA(
      Set(State("i"), State("<"), State("<<"), State("<<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol("<") -> Set(State("<"))),
                   State("<")   -> Map(Symbol("<") -> Set(State("<<"))),
                   State("<<")  -> Map(Symbol("=") -> Set(State("<<=")))
               )),
      State("i"),
      Set(State("<<="))
    ),

    TokenTypes.BinaryXor -> NFA(
      Set(State("i"), State("^")),
      Set(Symbol.epsilon, Symbol("^")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))))),
      State("i"),
      Set(State("^"))
    ),

    TokenTypes.BinaryXorAssign -> NFA(
      Set(State("i"), State("^"), State("^=")),
      Set(Symbol.epsilon, Symbol("^"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))),
                   State("^") -> Map(Symbol("=") -> Set(State("^=")))
               )),
      State("i"),
      Set(State("^="))
    ),

  TokenTypes.BinaryOr -> NFA(
      Set(State("i"), State("|")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))))),
      State("i"),
      Set(State("|"))
    ),

    TokenTypes.BinaryOrAssign -> NFA(
      Set(State("i"), State("|"), State("|=")),
      Set(Symbol.epsilon, Symbol("|"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("=") -> Set(State("|=")))
               )),
      State("i"),
      Set(State("|="))
    ),

    TokenTypes.LogicalOr -> NFA(
      Set(State("i"), State("|"), State("||")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("|") -> Set(State("||")))
               )),
      State("i"),
      Set(State("||"))
    ),

    TokenTypes.BinaryAnd -> NFA(
      Set(State("i"), State("&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))))),
      State("i"),
      Set(State("&"))
    ),

    TokenTypes.BinaryAndAssign -> NFA(
      Set(State("i"), State("&"), State("&=")),
      Set(Symbol.epsilon, Symbol("&"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("=") -> Set(State("&=")))
               )),
      State("i"),
      Set(State("&="))
    ),

    TokenTypes.LogicalAnd -> NFA(
      Set(State("i"), State("&"), State("&&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("&") -> Set(State("&&")))
               )),
      State("i"),
      Set(State("&&"))
    ),

    TokenTypes.Semicolon -> NFA(
      Set(State("i"), State(";")),
      Set(Symbol.epsilon, Symbol(";")),
      Relation(Map(State("i") -> Map(Symbol(";") -> Set(State(";"))))),
      State("i"),
      Set(State(";"))
    ),

    TokenTypes.Whitespace -> NFA(
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
      Set(State("ws"))
    ),

    TokenTypes.SingleLineComment -> NFA(
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
      Set(State("eol"), State("eol2"))
    ),

    TokenTypes.MultiLineComment -> NFA(
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
      Set(State("/**/"))
    ),

    TokenTypes.JavaDocComment -> NFA(
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
      Set(State("/***/"))
    ),

    TokenTypes.Num -> NFA(
      Set(State("i"), State("digit")),
      Set(Symbol.epsilon) ++ Symbol.digits,
      Relation(Map(State("i") -> Symbol.transitionsFromGroup(Symbol.digits, Set(State("digit"))),
                   State("digit") -> Symbol.transitionsFromGroup(Symbol.digits, Set(State("digit")))
               )),
      State("i"),
      Set(State("digit"))
    ),

    TokenTypes.Identifier -> NFA(
      Set(State("i"), State("id")),
      Set(Symbol.epsilon, Symbol("_"), Symbol("$")) ++ Symbol.letters ++ Symbol.digits,
      Relation(Map(State("i")   -> (Map(Symbol("_") -> Set(State("id")),
                                        Symbol("$") -> Set(State("id"))) ++
                                    Symbol.transitionsFromGroup(Symbol.letters, Set(State("id")))),
                   State("id")  -> (Map(Symbol("_") -> Set(State("id")),
                                        Symbol("$") -> Set(State("id"))) ++ 
                                    Symbol.transitionsFromGroup(Symbol.letters, Set(State("id"))) ++ 
                                    Symbol.transitionsFromGroup(Symbol.digits, Set(State("id"))))
               )),
      State("i"),
      Set(State("id"))
    ),

    TokenTypes.CharLiteral -> NFA(
      Set(State("i"), State("\'"), State("\'\\"), State("char-part"), State("char"),
          State("oct"), State("oct2"),
          State("part-oct"), State("part-oct2"), State("part-oct3")),
      Set(Symbol("\\"), Symbol("n"), Symbol("r"), Symbol("t"), Symbol("b"),
          Symbol("f"), Symbol("\""), Symbol("\'"), NegatedSymbols("\\", "\n", "\r")) ++ (
            Symbol.octalDigits ++ Symbol.quadDigits
          ),
      Relation(Map(State("i")     -> Map( Symbol("\'") -> Set(State("\'"))),
                   State("\'")    -> Map( Symbol("\\") -> Set(State("\'\\")),
                                          Symbol("\'") -> Set(State("char")),
                                          NegatedSymbols("\\", "\n", "\r") -> Set(State("char-part"))),
                   State("\'\\")  -> (Map( Symbol("n") -> Set(State("char-part")),
                                          Symbol("r") -> Set(State("char-part")),
                                          Symbol("t") -> Set(State("char-part")),
                                          Symbol("b") -> Set(State("char-part")),
                                          Symbol("f") -> Set(State("char-part")),
                                          Symbol("\"") -> Set(State("char-part")),
                                          Symbol("\'") -> Set(State("char-part"))) ++ (
                                            Symbol.transitionsFromGroup(Symbol.octalDigits, Set(State("oct"))) ++
                                            Symbol.transitionsFromGroup(Symbol.quadDigits, Set(State("part-oct")))
                                          )),
                   State("char-part") -> Map( Symbol("\'") -> Set(State("char"))),
                   State("oct")       -> (Map( Symbol("\'") -> Set(State("char"))) ++
                                         Symbol.transitionsFromGroup(Symbol.octalDigits, Set(State("oct2")))),
                   State("oct2")      -> Map( Symbol("\'") -> Set(State("char"))),
                   State("part-oct")  -> (Map( Symbol("\'") -> Set(State("char"))) ++
                                         Symbol.transitionsFromGroup(Symbol.octalDigits, Set(State("part-oct2")))),
                   State("part-oct2") -> (Map( Symbol("\'") -> Set(State("char"))) ++
                                         Symbol.transitionsFromGroup(Symbol.octalDigits, Set(State("part-oct3")))),
                   State("part-oct3") -> Map( Symbol("\'") -> Set(State("char")))
               )),
      State("i"),
      Set(State("char"))
    ),

    TokenTypes.StringLiteral -> NFA(
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
      Set(State("string"))
    )
  ) ++ keywords
}
