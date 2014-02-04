package joosbox.lexer
// WARNING: THIS IS AUTO-GENERATED CODE
// Don't update this file directly, unless you know
// for some reason that the joos-lexer-generator.py script
// will not be re-run.

object TokenTypes {

  object Question extends FixedTokenType {
    override def value = "?"
    override def apply(data: String = ""): Token = new Tokens.Question(verify(data))
  }

  object LeftParen extends FixedTokenType {
    override def value = "("
    override def apply(data: String = ""): Token = new Tokens.LeftParen(verify(data))
  }

  object RightParen extends FixedTokenType {
    override def value = ")"
    override def apply(data: String = ""): Token = new Tokens.RightParen(verify(data))
  }

  object LeftBracket extends FixedTokenType {
    override def value = "["
    override def apply(data: String = ""): Token = new Tokens.LeftBracket(verify(data))
  }

  object RightBracket extends FixedTokenType {
    override def value = "]"
    override def apply(data: String = ""): Token = new Tokens.RightBracket(verify(data))
  }

  object LeftCurly extends FixedTokenType {
    override def value = "{"
    override def apply(data: String = ""): Token = new Tokens.LeftCurly(verify(data))
  }

  object RightCurly extends FixedTokenType {
    override def value = "}"
    override def apply(data: String = ""): Token = new Tokens.RightCurly(verify(data))
  }

  object Colon extends FixedTokenType {
    override def value = ":"
    override def apply(data: String = ""): Token = new Tokens.Colon(verify(data))
  }

  object Comma extends FixedTokenType {
    override def value = ","
    override def apply(data: String = ""): Token = new Tokens.Comma(verify(data))
  }

  object Dot extends FixedTokenType {
    override def value = "."
    override def apply(data: String = ""): Token = new Tokens.Dot(verify(data))
  }

  object Assign extends FixedTokenType {
    override def value = "="
    override def apply(data: String = ""): Token = new Tokens.Assign(verify(data))
  }

  object Equal extends FixedTokenType {
    override def value = "=="
    override def apply(data: String = ""): Token = new Tokens.Equal(verify(data))
  }

  object LogicalNot extends FixedTokenType {
    override def value = "!"
    override def apply(data: String = ""): Token = new Tokens.LogicalNot(verify(data))
  }

  object BinaryNot extends FixedTokenType {
    override def value = "~"
    override def apply(data: String = ""): Token = new Tokens.BinaryNot(verify(data))
  }

  object NotEqual extends FixedTokenType {
    override def value = "!="
    override def apply(data: String = ""): Token = new Tokens.NotEqual(verify(data))
  }

  object Divide extends FixedTokenType {
    override def value = "/"
    override def apply(data: String = ""): Token = new Tokens.Divide(verify(data))
  }

  object DivideAssign extends FixedTokenType {
    override def value = "/="
    override def apply(data: String = ""): Token = new Tokens.DivideAssign(verify(data))
  }

  object Plus extends FixedTokenType {
    override def value = "+"
    override def apply(data: String = ""): Token = new Tokens.Plus(verify(data))
  }

  object PlusAssign extends FixedTokenType {
    override def value = "+="
    override def apply(data: String = ""): Token = new Tokens.PlusAssign(verify(data))
  }

  object Increment extends FixedTokenType {
    override def value = "++"
    override def apply(data: String = ""): Token = new Tokens.Increment(verify(data))
  }

  object Minus extends FixedTokenType {
    override def value = "-"
    override def apply(data: String = ""): Token = new Tokens.Minus(verify(data))
  }

  object MinusAssign extends FixedTokenType {
    override def value = "-="
    override def apply(data: String = ""): Token = new Tokens.MinusAssign(verify(data))
  }

  object Decrement extends FixedTokenType {
    override def value = "--"
    override def apply(data: String = ""): Token = new Tokens.Decrement(verify(data))
  }

  object Star extends FixedTokenType {
    override def value = "*"
    override def apply(data: String = ""): Token = new Tokens.Star(verify(data))
  }

  object StarAssign extends FixedTokenType {
    override def value = "*="
    override def apply(data: String = ""): Token = new Tokens.StarAssign(verify(data))
  }

  object Modulo extends FixedTokenType {
    override def value = "%"
    override def apply(data: String = ""): Token = new Tokens.Modulo(verify(data))
  }

  object ModuloAssign extends FixedTokenType {
    override def value = "%="
    override def apply(data: String = ""): Token = new Tokens.ModuloAssign(verify(data))
  }

  object GreaterThan extends FixedTokenType {
    override def value = ">"
    override def apply(data: String = ""): Token = new Tokens.GreaterThan(verify(data))
  }

  object GreaterEqual extends FixedTokenType {
    override def value = ">="
    override def apply(data: String = ""): Token = new Tokens.GreaterEqual(verify(data))
  }

  object ShiftRight extends FixedTokenType {
    override def value = ">>"
    override def apply(data: String = ""): Token = new Tokens.ShiftRight(verify(data))
  }

  object ShiftRightAssign extends FixedTokenType {
    override def value = ">>="
    override def apply(data: String = ""): Token = new Tokens.ShiftRightAssign(verify(data))
  }

  object BinaryShiftRight extends FixedTokenType {
    override def value = ">>>"
    override def apply(data: String = ""): Token = new Tokens.BinaryShiftRight(verify(data))
  }

  object BinaryShiftRightAssign extends FixedTokenType {
    override def value = ">>>="
    override def apply(data: String = ""): Token = new Tokens.BinaryShiftRightAssign(verify(data))
  }

  object LessThan extends FixedTokenType {
    override def value = "<"
    override def apply(data: String = ""): Token = new Tokens.LessThan(verify(data))
  }

  object LessEqual extends FixedTokenType {
    override def value = "<="
    override def apply(data: String = ""): Token = new Tokens.LessEqual(verify(data))
  }

  object ShiftLeft extends FixedTokenType {
    override def value = "<<"
    override def apply(data: String = ""): Token = new Tokens.ShiftLeft(verify(data))
  }

  object ShiftLeftAssign extends FixedTokenType {
    override def value = "<<="
    override def apply(data: String = ""): Token = new Tokens.ShiftLeftAssign(verify(data))
  }

  object BinaryXor extends FixedTokenType {
    override def value = "^"
    override def apply(data: String = ""): Token = new Tokens.BinaryXor(verify(data))
  }

  object BinaryXorAssign extends FixedTokenType {
    override def value = "^="
    override def apply(data: String = ""): Token = new Tokens.BinaryXorAssign(verify(data))
  }

  object BinaryOr extends FixedTokenType {
    override def value = "|"
    override def apply(data: String = ""): Token = new Tokens.BinaryOr(verify(data))
  }

  object BinaryOrAssign extends FixedTokenType {
    override def value = "|="
    override def apply(data: String = ""): Token = new Tokens.BinaryOrAssign(verify(data))
  }

  object LogicalOr extends FixedTokenType {
    override def value = "||"
    override def apply(data: String = ""): Token = new Tokens.LogicalOr(verify(data))
  }

  object BinaryAnd extends FixedTokenType {
    override def value = "&"
    override def apply(data: String = ""): Token = new Tokens.BinaryAnd(verify(data))
  }

  object BinaryAndAssign extends FixedTokenType {
    override def value = "&="
    override def apply(data: String = ""): Token = new Tokens.BinaryAndAssign(verify(data))
  }

  object LogicalAnd extends FixedTokenType {
    override def value = "&&"
    override def apply(data: String = ""): Token = new Tokens.LogicalAnd(verify(data))
  }

  object Semicolon extends FixedTokenType {
    override def value = ";"
    override def apply(data: String = ""): Token = new Tokens.Semicolon(verify(data))
  }

  object Whitespace extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.Whitespace(verify(data))
  }

  object SingleLineComment extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.SingleLineComment(verify(data))
  }

  object MultiLineComment extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.MultiLineComment(verify(data))
  }

  object JavaDocComment extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.JavaDocComment(verify(data))
  }

  object Num extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.Num(verify(data))
  }

  object CharLiteral extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.CharLiteral(verify(data))
  }

  object StringLiteral extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.StringLiteral(verify(data))
  }

  object Identifier extends VariableTokenType {
    override def apply(data: String = ""): Token = new Tokens.Identifier(verify(data))
  }

  object AbstractKeyword extends KeywordTokenType {
    override def value = "abstract"
    override def apply(data: String = ""): Token = new Tokens.AbstractKeyword(verify(data))
  }

  object BooleanKeyword extends KeywordTokenType {
    override def value = "boolean"
    override def apply(data: String = ""): Token = new Tokens.BooleanKeyword(verify(data))
  }

  object BreakKeyword extends KeywordTokenType {
    override def value = "break"
    override def apply(data: String = ""): Token = new Tokens.BreakKeyword(verify(data))
  }

  object ByteKeyword extends KeywordTokenType {
    override def value = "byte"
    override def apply(data: String = ""): Token = new Tokens.ByteKeyword(verify(data))
  }

  object CaseKeyword extends KeywordTokenType {
    override def value = "case"
    override def apply(data: String = ""): Token = new Tokens.CaseKeyword(verify(data))
  }

  object CatchKeyword extends KeywordTokenType {
    override def value = "catch"
    override def apply(data: String = ""): Token = new Tokens.CatchKeyword(verify(data))
  }

  object CharKeyword extends KeywordTokenType {
    override def value = "char"
    override def apply(data: String = ""): Token = new Tokens.CharKeyword(verify(data))
  }

  object ClassKeyword extends KeywordTokenType {
    override def value = "class"
    override def apply(data: String = ""): Token = new Tokens.ClassKeyword(verify(data))
  }

  object ConstKeyword extends KeywordTokenType {
    override def value = "const"
    override def apply(data: String = ""): Token = new Tokens.ConstKeyword(verify(data))
  }

  object ContinueKeyword extends KeywordTokenType {
    override def value = "continue"
    override def apply(data: String = ""): Token = new Tokens.ContinueKeyword(verify(data))
  }

  object DefaultKeyword extends KeywordTokenType {
    override def value = "default"
    override def apply(data: String = ""): Token = new Tokens.DefaultKeyword(verify(data))
  }

  object DoKeyword extends KeywordTokenType {
    override def value = "do"
    override def apply(data: String = ""): Token = new Tokens.DoKeyword(verify(data))
  }

  object DoubleKeyword extends KeywordTokenType {
    override def value = "double"
    override def apply(data: String = ""): Token = new Tokens.DoubleKeyword(verify(data))
  }

  object ElseKeyword extends KeywordTokenType {
    override def value = "else"
    override def apply(data: String = ""): Token = new Tokens.ElseKeyword(verify(data))
  }

  object ExtendsKeyword extends KeywordTokenType {
    override def value = "extends"
    override def apply(data: String = ""): Token = new Tokens.ExtendsKeyword(verify(data))
  }

  object FinalKeyword extends KeywordTokenType {
    override def value = "final"
    override def apply(data: String = ""): Token = new Tokens.FinalKeyword(verify(data))
  }

  object FinallyKeyword extends KeywordTokenType {
    override def value = "finally"
    override def apply(data: String = ""): Token = new Tokens.FinallyKeyword(verify(data))
  }

  object FloatKeyword extends KeywordTokenType {
    override def value = "float"
    override def apply(data: String = ""): Token = new Tokens.FloatKeyword(verify(data))
  }

  object ForKeyword extends KeywordTokenType {
    override def value = "for"
    override def apply(data: String = ""): Token = new Tokens.ForKeyword(verify(data))
  }

  object GotoKeyword extends KeywordTokenType {
    override def value = "goto"
    override def apply(data: String = ""): Token = new Tokens.GotoKeyword(verify(data))
  }

  object IfKeyword extends KeywordTokenType {
    override def value = "if"
    override def apply(data: String = ""): Token = new Tokens.IfKeyword(verify(data))
  }

  object ImplementsKeyword extends KeywordTokenType {
    override def value = "implements"
    override def apply(data: String = ""): Token = new Tokens.ImplementsKeyword(verify(data))
  }

  object ImportKeyword extends KeywordTokenType {
    override def value = "import"
    override def apply(data: String = ""): Token = new Tokens.ImportKeyword(verify(data))
  }

  object InstanceofKeyword extends KeywordTokenType {
    override def value = "instanceof"
    override def apply(data: String = ""): Token = new Tokens.InstanceofKeyword(verify(data))
  }

  object IntKeyword extends KeywordTokenType {
    override def value = "int"
    override def apply(data: String = ""): Token = new Tokens.IntKeyword(verify(data))
  }

  object InterfaceKeyword extends KeywordTokenType {
    override def value = "interface"
    override def apply(data: String = ""): Token = new Tokens.InterfaceKeyword(verify(data))
  }

  object LongKeyword extends KeywordTokenType {
    override def value = "long"
    override def apply(data: String = ""): Token = new Tokens.LongKeyword(verify(data))
  }

  object NativeKeyword extends KeywordTokenType {
    override def value = "native"
    override def apply(data: String = ""): Token = new Tokens.NativeKeyword(verify(data))
  }

  object NewKeyword extends KeywordTokenType {
    override def value = "new"
    override def apply(data: String = ""): Token = new Tokens.NewKeyword(verify(data))
  }

  object PackageKeyword extends KeywordTokenType {
    override def value = "package"
    override def apply(data: String = ""): Token = new Tokens.PackageKeyword(verify(data))
  }

  object PrivateKeyword extends KeywordTokenType {
    override def value = "private"
    override def apply(data: String = ""): Token = new Tokens.PrivateKeyword(verify(data))
  }

  object ProtectedKeyword extends KeywordTokenType {
    override def value = "protected"
    override def apply(data: String = ""): Token = new Tokens.ProtectedKeyword(verify(data))
  }

  object PublicKeyword extends KeywordTokenType {
    override def value = "public"
    override def apply(data: String = ""): Token = new Tokens.PublicKeyword(verify(data))
  }

  object ReturnKeyword extends KeywordTokenType {
    override def value = "return"
    override def apply(data: String = ""): Token = new Tokens.ReturnKeyword(verify(data))
  }

  object ShortKeyword extends KeywordTokenType {
    override def value = "short"
    override def apply(data: String = ""): Token = new Tokens.ShortKeyword(verify(data))
  }

  object StaticKeyword extends KeywordTokenType {
    override def value = "static"
    override def apply(data: String = ""): Token = new Tokens.StaticKeyword(verify(data))
  }

  object StrictfpKeyword extends KeywordTokenType {
    override def value = "strictfp"
    override def apply(data: String = ""): Token = new Tokens.StrictfpKeyword(verify(data))
  }

  object SuperKeyword extends KeywordTokenType {
    override def value = "super"
    override def apply(data: String = ""): Token = new Tokens.SuperKeyword(verify(data))
  }

  object SwitchKeyword extends KeywordTokenType {
    override def value = "switch"
    override def apply(data: String = ""): Token = new Tokens.SwitchKeyword(verify(data))
  }

  object SynchronizedKeyword extends KeywordTokenType {
    override def value = "synchronized"
    override def apply(data: String = ""): Token = new Tokens.SynchronizedKeyword(verify(data))
  }

  object ThisKeyword extends KeywordTokenType {
    override def value = "this"
    override def apply(data: String = ""): Token = new Tokens.ThisKeyword(verify(data))
  }

  object ThrowKeyword extends KeywordTokenType {
    override def value = "throw"
    override def apply(data: String = ""): Token = new Tokens.ThrowKeyword(verify(data))
  }

  object ThrowsKeyword extends KeywordTokenType {
    override def value = "throws"
    override def apply(data: String = ""): Token = new Tokens.ThrowsKeyword(verify(data))
  }

  object TransientKeyword extends KeywordTokenType {
    override def value = "transient"
    override def apply(data: String = ""): Token = new Tokens.TransientKeyword(verify(data))
  }

  object TryKeyword extends KeywordTokenType {
    override def value = "try"
    override def apply(data: String = ""): Token = new Tokens.TryKeyword(verify(data))
  }

  object VoidKeyword extends KeywordTokenType {
    override def value = "void"
    override def apply(data: String = ""): Token = new Tokens.VoidKeyword(verify(data))
  }

  object VolatileKeyword extends KeywordTokenType {
    override def value = "volatile"
    override def apply(data: String = ""): Token = new Tokens.VolatileKeyword(verify(data))
  }

  object WhileKeyword extends KeywordTokenType {
    override def value = "while"
    override def apply(data: String = ""): Token = new Tokens.WhileKeyword(verify(data))
  }

  object TrueLiteral extends KeywordTokenType {
    override def value = "true"
    override def apply(data: String = ""): Token = new Tokens.TrueLiteral(verify(data))
  }

  object FalseLiteral extends KeywordTokenType {
    override def value = "false"
    override def apply(data: String = ""): Token = new Tokens.FalseLiteral(verify(data))
  }

  object NullLiteral extends KeywordTokenType {
    override def value = "null"
    override def apply(data: String = ""): Token = new Tokens.NullLiteral(verify(data))
  }

  val Keywords = Map(
    "false" -> TokenTypes.FalseLiteral, 
    "synchronized" -> TokenTypes.SynchronizedKeyword, 
    "int" -> TokenTypes.IntKeyword, 
    "abstract" -> TokenTypes.AbstractKeyword, 
    "float" -> TokenTypes.FloatKeyword, 
    "private" -> TokenTypes.PrivateKeyword, 
    "char" -> TokenTypes.CharKeyword, 
    "interface" -> TokenTypes.InterfaceKeyword, 
    "boolean" -> TokenTypes.BooleanKeyword, 
    "static" -> TokenTypes.StaticKeyword, 
    "null" -> TokenTypes.NullLiteral, 
    "if" -> TokenTypes.IfKeyword, 
    "const" -> TokenTypes.ConstKeyword, 
    "for" -> TokenTypes.ForKeyword, 
    "true" -> TokenTypes.TrueLiteral, 
    "while" -> TokenTypes.WhileKeyword, 
    "long" -> TokenTypes.LongKeyword, 
    "throw" -> TokenTypes.ThrowKeyword, 
    "strictfp" -> TokenTypes.StrictfpKeyword, 
    "finally" -> TokenTypes.FinallyKeyword, 
    "protected" -> TokenTypes.ProtectedKeyword, 
    "extends" -> TokenTypes.ExtendsKeyword, 
    "implements" -> TokenTypes.ImplementsKeyword, 
    "import" -> TokenTypes.ImportKeyword, 
    "native" -> TokenTypes.NativeKeyword, 
    "final" -> TokenTypes.FinalKeyword, 
    "do" -> TokenTypes.DoKeyword, 
    "return" -> TokenTypes.ReturnKeyword, 
    "goto" -> TokenTypes.GotoKeyword, 
    "void" -> TokenTypes.VoidKeyword, 
    "else" -> TokenTypes.ElseKeyword, 
    "break" -> TokenTypes.BreakKeyword, 
    "transient" -> TokenTypes.TransientKeyword, 
    "new" -> TokenTypes.NewKeyword, 
    "catch" -> TokenTypes.CatchKeyword, 
    "instanceof" -> TokenTypes.InstanceofKeyword, 
    "byte" -> TokenTypes.ByteKeyword, 
    "super" -> TokenTypes.SuperKeyword, 
    "class" -> TokenTypes.ClassKeyword, 
    "volatile" -> TokenTypes.VolatileKeyword, 
    "case" -> TokenTypes.CaseKeyword, 
    "short" -> TokenTypes.ShortKeyword, 
    "package" -> TokenTypes.PackageKeyword, 
    "default" -> TokenTypes.DefaultKeyword, 
    "double" -> TokenTypes.DoubleKeyword, 
    "public" -> TokenTypes.PublicKeyword, 
    "try" -> TokenTypes.TryKeyword, 
    "this" -> TokenTypes.ThisKeyword, 
    "switch" -> TokenTypes.SwitchKeyword, 
    "continue" -> TokenTypes.ContinueKeyword, 
    "throws" -> TokenTypes.ThrowsKeyword 
  )
}




object Tokens {
  //  Classes of token objects themselves - these are instantiated.

  case class Question(override val data: String) extends FixedToken(data)
  case class LeftParen(override val data: String) extends FixedToken(data)
  case class RightParen(override val data: String) extends FixedToken(data)
  case class LeftBracket(override val data: String) extends FixedToken(data)
  case class RightBracket(override val data: String) extends FixedToken(data)
  case class LeftCurly(override val data: String) extends FixedToken(data)
  case class RightCurly(override val data: String) extends FixedToken(data)
  case class Colon(override val data: String) extends FixedToken(data)
  case class Comma(override val data: String) extends FixedToken(data)
  case class Dot(override val data: String) extends FixedToken(data)
  case class Assign(override val data: String) extends FixedToken(data)
  case class Equal(override val data: String) extends FixedToken(data)
  case class LogicalNot(override val data: String) extends FixedToken(data)
  case class BinaryNot(override val data: String) extends FixedToken(data)
  case class NotEqual(override val data: String) extends FixedToken(data)
  case class Divide(override val data: String) extends FixedToken(data)
  case class DivideAssign(override val data: String) extends FixedToken(data)
  case class Plus(override val data: String) extends FixedToken(data)
  case class PlusAssign(override val data: String) extends FixedToken(data)
  case class Increment(override val data: String) extends FixedToken(data)
  case class Minus(override val data: String) extends FixedToken(data)
  case class MinusAssign(override val data: String) extends FixedToken(data)
  case class Decrement(override val data: String) extends FixedToken(data)
  case class Star(override val data: String) extends FixedToken(data)
  case class StarAssign(override val data: String) extends FixedToken(data)
  case class Modulo(override val data: String) extends FixedToken(data)
  case class ModuloAssign(override val data: String) extends FixedToken(data)
  case class GreaterThan(override val data: String) extends FixedToken(data)
  case class GreaterEqual(override val data: String) extends FixedToken(data)
  case class ShiftRight(override val data: String) extends FixedToken(data)
  case class ShiftRightAssign(override val data: String) extends FixedToken(data)
  case class BinaryShiftRight(override val data: String) extends FixedToken(data)
  case class BinaryShiftRightAssign(override val data: String) extends FixedToken(data)
  case class LessThan(override val data: String) extends FixedToken(data)
  case class LessEqual(override val data: String) extends FixedToken(data)
  case class ShiftLeft(override val data: String) extends FixedToken(data)
  case class ShiftLeftAssign(override val data: String) extends FixedToken(data)
  case class BinaryXor(override val data: String) extends FixedToken(data)
  case class BinaryXorAssign(override val data: String) extends FixedToken(data)
  case class BinaryOr(override val data: String) extends FixedToken(data)
  case class BinaryOrAssign(override val data: String) extends FixedToken(data)
  case class LogicalOr(override val data: String) extends FixedToken(data)
  case class BinaryAnd(override val data: String) extends FixedToken(data)
  case class BinaryAndAssign(override val data: String) extends FixedToken(data)
  case class LogicalAnd(override val data: String) extends FixedToken(data)
  case class Semicolon(override val data: String) extends FixedToken(data)
  case class Whitespace(override val data: String) extends VariableToken(data)
  case class SingleLineComment(override val data: String) extends VariableToken(data)
  case class MultiLineComment(override val data: String) extends VariableToken(data)
  case class JavaDocComment(override val data: String) extends VariableToken(data)
  case class Num(override val data: String) extends VariableToken(data)
  case class CharLiteral(override val data: String) extends VariableToken(data)
  case class StringLiteral(override val data: String) extends VariableToken(data)
  case class Identifier(override val data: String) extends VariableToken(data)
  case class AbstractKeyword(override val data: String) extends KeywordToken(data)
  case class BooleanKeyword(override val data: String) extends KeywordToken(data)
  case class BreakKeyword(override val data: String) extends KeywordToken(data)
  case class ByteKeyword(override val data: String) extends KeywordToken(data)
  case class CaseKeyword(override val data: String) extends KeywordToken(data)
  case class CatchKeyword(override val data: String) extends KeywordToken(data)
  case class CharKeyword(override val data: String) extends KeywordToken(data)
  case class ClassKeyword(override val data: String) extends KeywordToken(data)
  case class ConstKeyword(override val data: String) extends KeywordToken(data)
  case class ContinueKeyword(override val data: String) extends KeywordToken(data)
  case class DefaultKeyword(override val data: String) extends KeywordToken(data)
  case class DoKeyword(override val data: String) extends KeywordToken(data)
  case class DoubleKeyword(override val data: String) extends KeywordToken(data)
  case class ElseKeyword(override val data: String) extends KeywordToken(data)
  case class ExtendsKeyword(override val data: String) extends KeywordToken(data)
  case class FinalKeyword(override val data: String) extends KeywordToken(data)
  case class FinallyKeyword(override val data: String) extends KeywordToken(data)
  case class FloatKeyword(override val data: String) extends KeywordToken(data)
  case class ForKeyword(override val data: String) extends KeywordToken(data)
  case class GotoKeyword(override val data: String) extends KeywordToken(data)
  case class IfKeyword(override val data: String) extends KeywordToken(data)
  case class ImplementsKeyword(override val data: String) extends KeywordToken(data)
  case class ImportKeyword(override val data: String) extends KeywordToken(data)
  case class InstanceofKeyword(override val data: String) extends KeywordToken(data)
  case class IntKeyword(override val data: String) extends KeywordToken(data)
  case class InterfaceKeyword(override val data: String) extends KeywordToken(data)
  case class LongKeyword(override val data: String) extends KeywordToken(data)
  case class NativeKeyword(override val data: String) extends KeywordToken(data)
  case class NewKeyword(override val data: String) extends KeywordToken(data)
  case class PackageKeyword(override val data: String) extends KeywordToken(data)
  case class PrivateKeyword(override val data: String) extends KeywordToken(data)
  case class ProtectedKeyword(override val data: String) extends KeywordToken(data)
  case class PublicKeyword(override val data: String) extends KeywordToken(data)
  case class ReturnKeyword(override val data: String) extends KeywordToken(data)
  case class ShortKeyword(override val data: String) extends KeywordToken(data)
  case class StaticKeyword(override val data: String) extends KeywordToken(data)
  case class StrictfpKeyword(override val data: String) extends KeywordToken(data)
  case class SuperKeyword(override val data: String) extends KeywordToken(data)
  case class SwitchKeyword(override val data: String) extends KeywordToken(data)
  case class SynchronizedKeyword(override val data: String) extends KeywordToken(data)
  case class ThisKeyword(override val data: String) extends KeywordToken(data)
  case class ThrowKeyword(override val data: String) extends KeywordToken(data)
  case class ThrowsKeyword(override val data: String) extends KeywordToken(data)
  case class TransientKeyword(override val data: String) extends KeywordToken(data)
  case class TryKeyword(override val data: String) extends KeywordToken(data)
  case class VoidKeyword(override val data: String) extends KeywordToken(data)
  case class VolatileKeyword(override val data: String) extends KeywordToken(data)
  case class WhileKeyword(override val data: String) extends KeywordToken(data)
  case class TrueLiteral(override val data: String) extends KeywordToken(data)
  case class FalseLiteral(override val data: String) extends KeywordToken(data)
  case class NullLiteral(override val data: String) extends KeywordToken(data)
}
