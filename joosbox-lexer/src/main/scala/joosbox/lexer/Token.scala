package joosbox.lexer

import scala.util.matching.Regex

object Token {
  trait Kind {
    val data: String

    def consume(data: String): Kind = {
      //  For any of the below case classes, we can create a new instance of
      //  the case class by calling consume on it with the token that it should contain.

      val constructor = this.getClass.getConstructors.head
      val args = Array[AnyRef](data)
      constructor.newInstance(args: _*).asInstanceOf[Token.Kind].resolve
    }

    def tokenKind(): String = {
      data match {
        case "" => toString.replace("()", "")
        case _ => data
      }
    }

    def resolve: Token.Kind = this
  }

  case class Question(data: String) extends Kind
  case class LeftParen(data: String) extends Kind
  case class RightParen(data: String) extends Kind
  case class LeftBracket(data: String) extends Kind
  case class RightBracket(data: String) extends Kind
  case class LeftCurly(data: String) extends Kind
  case class RightCurly(data: String) extends Kind
  case class Colon(data: String) extends Kind
  case class Comma(data: String) extends Kind
  case class Dot(data: String) extends Kind
  case class Assign(data: String) extends Kind
  case class Equal(data: String) extends Kind
  case class LogicalNot(data: String) extends Kind
  case class BinaryNot(data: String) extends Kind
  case class NotEqual(data: String) extends Kind
  case class Divide(data: String) extends Kind
  case class DivideAssign(data: String) extends Kind
  case class Plus(data: String) extends Kind
  case class PlusAssign(data: String) extends Kind
  case class Increment(data: String) extends Kind
  case class Minus(data: String) extends Kind
  case class MinusAssign(data: String) extends Kind
  case class Decrement(data: String) extends Kind
  case class Star(data: String) extends Kind
  case class StarAssign(data: String) extends Kind
  case class Modulo(data: String) extends Kind
  case class ModuloAssign(data: String) extends Kind
  case class GreaterThan(data: String) extends Kind
  case class GreaterEqual(data: String) extends Kind
  case class ShiftRight(data: String) extends Kind
  case class ShiftRightAssign(data: String) extends Kind
  case class BinaryShiftRight(data: String) extends Kind
  case class BinaryShiftRightAssign(data: String) extends Kind
  case class LessThan(data: String) extends Kind
  case class LessEqual(data: String) extends Kind
  case class ShiftLeft(data: String) extends Kind
  case class ShiftLeftAssign(data: String) extends Kind
  case class BinaryXor(data: String) extends Kind
  case class BinaryXorAssign(data: String) extends Kind
  case class BinaryOr(data: String) extends Kind
  case class BinaryOrAssign(data: String) extends Kind
  case class LogicalOr(data: String) extends Kind
  case class BinaryAnd(data: String) extends Kind
  case class BinaryAndAssign(data: String) extends Kind
  case class LogicalAnd(data: String) extends Kind
  case class Semicolon(data: String) extends Kind

  case class Whitespace(data: String) extends Kind
  case class SingleLineComment(data: String) extends Kind
  case class MultiLineComment(data: String) extends Kind
  case class JavaDocComment(data: String) extends Kind

  case class Num(data: String) extends Kind
  case class CharLiteral(data: String) extends Kind
  case class StringLiteral(data: String) extends Kind

  case class AbstractKeyword(data: String) extends Kind
  case class BooleanKeyword(data: String) extends Kind
  case class BreakKeyword(data: String) extends Kind
  case class ByteKeyword(data: String) extends Kind
  case class CaseKeyword(data: String) extends Kind
  case class CatchKeyword(data: String) extends Kind
  case class CharKeyword(data: String) extends Kind
  case class ClassKeyword(data: String) extends Kind
  case class ConstKeyword(data: String) extends Kind
  case class ContinueKeyword(data: String) extends Kind
  case class DefaultKeyword(data: String) extends Kind
  case class DoKeyword(data: String) extends Kind
  case class DoubleKeyword(data: String) extends Kind
  case class ElseKeyword(data: String) extends Kind
  case class ExtendsKeyword(data: String) extends Kind
  case class FinalKeyword(data: String) extends Kind
  case class FinallyKeyword(data: String) extends Kind
  case class FloatKeyword(data: String) extends Kind
  case class ForKeyword(data: String) extends Kind
  case class GotoKeyword(data: String) extends Kind
  case class IfKeyword(data: String) extends Kind
  case class ImplementsKeyword(data: String) extends Kind
  case class ImportKeyword(data: String) extends Kind
  case class InstanceofKeyword(data: String) extends Kind
  case class IntKeyword(data: String) extends Kind
  case class InterfaceKeyword(data: String) extends Kind
  case class LongKeyword(data: String) extends Kind
  case class NativeKeyword(data: String) extends Kind
  case class NewKeyword(data: String) extends Kind
  case class PackageKeyword(data: String) extends Kind
  case class PrivateKeyword(data: String) extends Kind
  case class ProtectedKeyword(data: String) extends Kind
  case class PublicKeyword(data: String) extends Kind
  case class ReturnKeyword(data: String) extends Kind
  case class ShortKeyword(data: String) extends Kind
  case class StaticKeyword(data: String) extends Kind
  case class StrictfpKeyword(data: String) extends Kind
  case class SuperKeyword(data: String) extends Kind
  case class SwitchKeyword(data: String) extends Kind
  case class SynchronizedKeyword(data: String) extends Kind
  case class ThisKeyword(data: String) extends Kind
  case class ThrowKeyword(data: String) extends Kind
  case class ThrowsKeyword(data: String) extends Kind
  case class TransientKeyword(data: String) extends Kind
  case class TryKeyword(data: String) extends Kind
  case class VoidKeyword(data: String) extends Kind
  case class VolatileKeyword(data: String) extends Kind
  case class WhileKeyword(data: String) extends Kind

  case class TrueLiteral(data: String) extends Kind
  case class FalseLiteral(data: String) extends Kind
  case class NullLiteral(data: String) extends Kind

  case class Identifier(data: String) extends Kind {
    override def resolve: Token.Kind = data match {
      case "abstract" =>      Token.AbstractKeyword(data)
      case "boolean" =>       Token.BooleanKeyword(data)
      case "break" =>         Token.BreakKeyword(data)
      case "byte" =>          Token.ByteKeyword(data)
      case "case" =>          Token.CaseKeyword(data)
      case "catch" =>         Token.CatchKeyword(data)
      case "char" =>          Token.CharKeyword(data)
      case "class" =>         Token.ClassKeyword(data)
      case "const" =>         Token.ConstKeyword(data)
      case "continue" =>      Token.ContinueKeyword(data)
      case "default" =>       Token.DefaultKeyword(data)
      case "do" =>            Token.DoKeyword(data)
      case "double" =>        Token.DoubleKeyword(data)
      case "else" =>          Token.ElseKeyword(data)
      case "extends" =>       Token.ExtendsKeyword(data)
      case "final" =>         Token.FinalKeyword(data)
      case "finally" =>       Token.FinallyKeyword(data)
      case "float" =>         Token.FloatKeyword(data)
      case "for" =>           Token.ForKeyword(data)
      case "goto" =>          Token.GotoKeyword(data)
      case "if" =>            Token.IfKeyword(data)
      case "implements" =>    Token.ImplementsKeyword(data)
      case "import" =>        Token.ImportKeyword(data)
      case "instanceof" =>    Token.InstanceofKeyword(data)
      case "int" =>           Token.IntKeyword(data)
      case "interface" =>     Token.InterfaceKeyword(data)
      case "long" =>          Token.LongKeyword(data)
      case "native" =>        Token.NativeKeyword(data)
      case "new" =>           Token.NewKeyword(data)
      case "package" =>       Token.PackageKeyword(data)
      case "private" =>       Token.PrivateKeyword(data)
      case "protected" =>     Token.ProtectedKeyword(data)
      case "public" =>        Token.PublicKeyword(data)
      case "return" =>        Token.ReturnKeyword(data)
      case "short" =>         Token.ShortKeyword(data)
      case "static" =>        Token.StaticKeyword(data)
      case "strictfp" =>      Token.StrictfpKeyword(data)
      case "super" =>         Token.SuperKeyword(data)
      case "switch" =>        Token.SwitchKeyword(data)
      case "synchronized" =>  Token.SynchronizedKeyword(data)
      case "this" =>          Token.ThisKeyword(data)
      case "throw" =>         Token.ThrowKeyword(data)
      case "throws" =>        Token.ThrowsKeyword(data)
      case "transient" =>     Token.TransientKeyword(data)
      case "try" =>           Token.TryKeyword(data)
      case "void" =>          Token.VoidKeyword(data)
      case "volatile" =>      Token.VolatileKeyword(data)
      case "while" =>         Token.WhileKeyword(data)
  
      case "true" =>          Token.TrueLiteral(data)
      case "false" =>         Token.FalseLiteral(data)
      case "null" =>          Token.NullLiteral(data)

      case _ => this
    }
  }

  //  Dummy tokens for our NFAs
  case class Test(data: String) extends Kind
  case class Combined(data: String) extends Kind
}

object TokenNFA {
  lazy val nfa: NFA = NFA.union(nfas.values.toSet)

  lazy val nfas = rawNFAs.map {
    case (token, nfa: NFA) => token -> nfa.withToken(token.apply(""))
  }

  private val rawNFAs = Map(
    Token.Question -> NFA(
      Set(State("i"), State("?")),
      Set(Symbol.epsilon, Symbol("?")),
      Relation(Map(State("i") -> Map(Symbol("?") -> Set(State("?"))))),
      State("i"),
      Set(State("?"))
    ),

    Token.LeftParen -> NFA(
      Set(State("i"), State("(")),
      Set(Symbol.epsilon, Symbol("(")),
      Relation(Map(State("i") -> Map(Symbol("(") -> Set(State("("))))),
      State("i"),
      Set(State("("))
    ),

    Token.RightParen -> NFA(
      Set(State("i"), State(")")),
      Set(Symbol.epsilon, Symbol(")")),
      Relation(Map(State("i") -> Map(Symbol(")") -> Set(State(")"))))),
      State("i"),
      Set(State(")"))
    ),

    Token.LeftBracket -> NFA(
      Set(State("i"), State("[")),
      Set(Symbol.epsilon, Symbol("[")),
      Relation(Map(State("i") -> Map(Symbol("[") -> Set(State("["))))),
      State("i"),
      Set(State("["))
    ),

    Token.RightBracket -> NFA(
      Set(State("i"), State("]")),
      Set(Symbol.epsilon, Symbol("]")),
      Relation(Map(State("i") -> Map(Symbol("]") -> Set(State("]"))))),
      State("i"),
      Set(State("]"))
    ),

    Token.LeftCurly -> NFA(
      Set(State("i"), State("{")),
      Set(Symbol.epsilon, Symbol("{")),
      Relation(Map(State("i") -> Map(Symbol("{") -> Set(State("{"))))),
      State("i"),
      Set(State("{"))
    ),

    Token.RightCurly -> NFA(
      Set(State("i"), State("}")),
      Set(Symbol.epsilon, Symbol("}")),
      Relation(Map(State("i") -> Map(Symbol("}") -> Set(State("}"))))),
      State("i"),
      Set(State("}"))
    ),

    Token.Colon -> NFA(
      Set(State("i"), State(":")),
      Set(Symbol.epsilon, Symbol(":")),
      Relation(Map(State("i") -> Map(Symbol(":") -> Set(State(":"))))),
      State("i"),
      Set(State(":"))
    ),

    Token.Comma -> NFA(
      Set(State("i"), State(",")),
      Set(Symbol.epsilon, Symbol(",")),
      Relation(Map(State("i") -> Map(Symbol(",") -> Set(State(","))))),
      State("i"),
      Set(State(","))
    ),

    Token.Dot -> NFA(
      Set(State("i"), State(".")),
      Set(Symbol.epsilon, Symbol(".")),
      Relation(Map(State("i") -> Map(Symbol(".") -> Set(State("."))))),
      State("i"),
      Set(State("."))
    ),

    Token.Assign -> NFA(
      Set(State("i"), State("=")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))))),
      State("i"),
      Set(State("="))
    ),

    Token.Equal -> NFA(
      Set(State("i"), State("="), State("==")),
      Set(Symbol.epsilon, Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("=") -> Set(State("="))),
                   State("=") -> Map(Symbol("=") -> Set(State("==")))
               )),
      State("i"),
      Set(State("=="))
    ),

    Token.LogicalNot -> NFA(
      Set(State("i"), State("!")),
      Set(Symbol.epsilon, Symbol("!")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))))),
      State("i"),
      Set(State("!"))
    ),

    Token.BinaryNot -> NFA(
      Set(State("i"), State("~")),
      Set(Symbol.epsilon, Symbol("~")),
      Relation(Map(State("i") -> Map(Symbol("~") -> Set(State("~"))))),
      State("i"),
      Set(State("~"))
    ),

    Token.NotEqual -> NFA(
      Set(State("i"), State("!"), State("!=")),
      Set(Symbol.epsilon, Symbol("!"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("!") -> Set(State("!"))),
                   State("!") -> Map(Symbol("=") -> Set(State("!=")))
               )),
      State("i"),
      Set(State("!="))
    ),

    Token.Divide -> NFA(
      Set(State("i"), State("/")),
      Set(Symbol.epsilon, Symbol("/")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))))),
      State("i"),
      Set(State("/"))
    ),

    Token.DivideAssign -> NFA(
      Set(State("i"), State("/"), State("/=")),
      Set(Symbol.epsilon, Symbol("/"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("/") -> Set(State("/"))),
                   State("/") -> Map(Symbol("=") -> Set(State("/=")))
               )),
      State("i"),
      Set(State("/="))
    ),

    Token.Plus -> NFA(
      Set(State("i"), State("+")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))))),
      State("i"),
      Set(State("+"))
    ),

    Token.PlusAssign -> NFA(
      Set(State("i"), State("+"), State("+=")),
      Set(Symbol.epsilon, Symbol("+"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("=") -> Set(State("+=")))
               )),
      State("i"),
      Set(State("+="))
    ),

    Token.Increment -> NFA(
      Set(State("i"), State("+"), State("++")),
      Set(Symbol.epsilon, Symbol("+")),
      Relation(Map(State("i") -> Map(Symbol("+") -> Set(State("+"))),
                   State("+") -> Map(Symbol("+") -> Set(State("++")))
               )),
      State("i"),
      Set(State("++"))
    ),

    Token.Minus -> NFA(
      Set(State("i"), State("-")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))))),
      State("i"),
      Set(State("-"))
    ),

    Token.MinusAssign -> NFA(
      Set(State("i"), State("-"), State("-=")),
      Set(Symbol.epsilon, Symbol("-"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("=") -> Set(State("-=")))
               )),
      State("i"),
      Set(State("-="))
    ),

    Token.Decrement -> NFA(
      Set(State("i"), State("-"), State("--")),
      Set(Symbol.epsilon, Symbol("-")),
      Relation(Map(State("i") -> Map(Symbol("-") -> Set(State("-"))),
                   State("-") -> Map(Symbol("-") -> Set(State("--")))
               )),
      State("i"),
      Set(State("--"))
    ),

    Token.Star -> NFA(
      Set(State("i"), State("*")),
      Set(Symbol.epsilon, Symbol("*")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))))),
      State("i"),
      Set(State("*"))
    ),

    Token.StarAssign -> NFA(
      Set(State("i"), State("*"), State("*=")),
      Set(Symbol.epsilon, Symbol("*"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("*") -> Set(State("*"))),
                   State("*") -> Map(Symbol("=") -> Set(State("*=")))
               )),
      State("i"),
      Set(State("*="))
    ),

    Token.Modulo -> NFA(
      Set(State("i"), State("%")),
      Set(Symbol.epsilon, Symbol("%")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))))),
      State("i"),
      Set(State("%"))
    ),

    Token.ModuloAssign -> NFA(
      Set(State("i"), State("%"), State("%=")),
      Set(Symbol.epsilon, Symbol("%"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("%") -> Set(State("%"))),
                   State("%") -> Map(Symbol("=") -> Set(State("%=")))
               )),
      State("i"),
      Set(State("%="))
    ),

    Token.GreaterThan -> NFA(
      Set(State("i"), State(">")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))))),
      State("i"),
      Set(State(">"))
    ),

    Token.GreaterEqual -> NFA(
      Set(State("i"), State(">"), State(">=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol("=") -> Set(State(">=")))
               )),
      State("i"),
      Set(State(">="))
    ),

    Token.ShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i") -> Map(Symbol(">") -> Set(State(">"))),
                   State(">") -> Map(Symbol(">") -> Set(State(">>")))
               )),
      State("i"),
      Set(State(">>"))
    ),

    Token.ShiftRightAssign -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>=")),
      Set(Symbol.epsilon, Symbol(">"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol("=") -> Set(State(">>=")))
               )),
      State("i"),
      Set(State(">>="))
    ),

    Token.BinaryShiftRight -> NFA(
      Set(State("i"), State(">"), State(">>"), State(">>>")),
      Set(Symbol.epsilon, Symbol(">")),
      Relation(Map(State("i")   -> Map(Symbol(">") -> Set(State(">"))),
                   State(">")   -> Map(Symbol(">") -> Set(State(">>"))),
                   State(">>")  -> Map(Symbol(">") -> Set(State(">>>")))
               )),
      State("i"),
      Set(State(">>>"))
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
      Set(State(">>>="))
    ),

    Token.LessThan -> NFA(
      Set(State("i"), State("<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))))),
      State("i"),
      Set(State("<"))
    ),

    Token.LessEqual -> NFA(
      Set(State("i"), State("<"), State("<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("=") -> Set(State("<=")))
               )),
      State("i"),
      Set(State("<="))
    ),

    Token.ShiftLeft -> NFA(
      Set(State("i"), State("<"), State("<<")),
      Set(Symbol.epsilon, Symbol("<")),
      Relation(Map(State("i") -> Map(Symbol("<") -> Set(State("<"))),
                   State("<") -> Map(Symbol("<") -> Set(State("<<")))
               )),
      State("i"),
      Set(State("<<"))
    ),

    Token.ShiftLeftAssign -> NFA(
      Set(State("i"), State("<"), State("<<"), State("<<=")),
      Set(Symbol.epsilon, Symbol("<"), Symbol("=")),
      Relation(Map(State("i")   -> Map(Symbol("<") -> Set(State("<"))),
                   State("<")   -> Map(Symbol("<") -> Set(State("<<"))),
                   State("<<")  -> Map(Symbol("=") -> Set(State("<<=")))
               )),
      State("i"),
      Set(State("<<="))
    ),

    Token.BinaryXor -> NFA(
      Set(State("i"), State("^")),
      Set(Symbol.epsilon, Symbol("^")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))))),
      State("i"),
      Set(State("^"))
    ),

    Token.BinaryXorAssign -> NFA(
      Set(State("i"), State("^"), State("^=")),
      Set(Symbol.epsilon, Symbol("^"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("^") -> Set(State("^"))),
                   State("^") -> Map(Symbol("=") -> Set(State("^=")))
               )),
      State("i"),
      Set(State("^="))
    ),

  Token.BinaryOr -> NFA(
      Set(State("i"), State("|")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))))),
      State("i"),
      Set(State("|"))
    ),

    Token.BinaryOrAssign -> NFA(
      Set(State("i"), State("|"), State("|=")),
      Set(Symbol.epsilon, Symbol("|"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("=") -> Set(State("|=")))
               )),
      State("i"),
      Set(State("|="))
    ),

    Token.LogicalOr -> NFA(
      Set(State("i"), State("|"), State("||")),
      Set(Symbol.epsilon, Symbol("|")),
      Relation(Map(State("i") -> Map(Symbol("|") -> Set(State("|"))),
                   State("|") -> Map(Symbol("|") -> Set(State("||")))
               )),
      State("i"),
      Set(State("||"))
    ),

    Token.BinaryAnd -> NFA(
      Set(State("i"), State("&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))))),
      State("i"),
      Set(State("&"))
    ),

    Token.BinaryAndAssign -> NFA(
      Set(State("i"), State("&"), State("&=")),
      Set(Symbol.epsilon, Symbol("&"), Symbol("=")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("=") -> Set(State("&=")))
               )),
      State("i"),
      Set(State("&="))
    ),

    Token.LogicalAnd -> NFA(
      Set(State("i"), State("&"), State("&&")),
      Set(Symbol.epsilon, Symbol("&")),
      Relation(Map(State("i") -> Map(Symbol("&") -> Set(State("&"))),
                   State("&") -> Map(Symbol("&") -> Set(State("&&")))
               )),
      State("i"),
      Set(State("&&"))
    ),

    Token.Semicolon -> NFA(
      Set(State("i"), State(";")),
      Set(Symbol.epsilon, Symbol(";")),
      Relation(Map(State("i") -> Map(Symbol(";") -> Set(State(";"))))),
      State("i"),
      Set(State(";"))
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
      Set(State("ws"))
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
      Set(State("eol"), State("eol2"))
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
      Set(State("/**/"))
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
      Set(State("/***/"))
    ),

    Token.Num -> NFA(
      Set(State("i"), State("digit")),
      Set(Symbol.epsilon, Symbol.digitsGroup),
      Relation(Map(State("i") -> Map(Symbol.digitsGroup -> Set(State("digit"))),
                   State("digit") -> Map(Symbol.digitsGroup -> Set(State("digit")))
               )),
      State("i"),
      Set(State("digit"))
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
      Set(State("id"))
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
      Set(State("char"))
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
      Set(State("string"))
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
