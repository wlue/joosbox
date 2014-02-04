package joosbox.lexer
// WARNING: THIS IS AUTO-GENERATED CODE
// Don't update this file directly, unless you know
// for some reason that the joos-lexer-generator.py script
// will not be re-run.

object TokenTypes {

  object BOF extends FixedTokenType {
    override def value = ""
    override def apply(data: String = ""): Token = new Tokens.BOF(verify(data))
  }

  object EOF extends FixedTokenType {
    override def value = ""
    override def apply(data: String = ""): Token = new Tokens.EOF(verify(data))
  }

  object S extends FixedTokenType {
    override def value = ""
    override def apply(data: String = ""): Token = new Tokens.S(verify(data))
  }

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
  def fromString(input: String): TokenType = input match {
    case "BOF" => BOF
    case "EOF" => EOF
    case "S" => S
    case "Question" => Question
    case "LeftParen" => LeftParen
    case "RightParen" => RightParen
    case "LeftBracket" => LeftBracket
    case "RightBracket" => RightBracket
    case "LeftCurly" => LeftCurly
    case "RightCurly" => RightCurly
    case "Colon" => Colon
    case "Comma" => Comma
    case "Dot" => Dot
    case "Assign" => Assign
    case "Equal" => Equal
    case "LogicalNot" => LogicalNot
    case "BinaryNot" => BinaryNot
    case "NotEqual" => NotEqual
    case "Divide" => Divide
    case "DivideAssign" => DivideAssign
    case "Plus" => Plus
    case "PlusAssign" => PlusAssign
    case "Increment" => Increment
    case "Minus" => Minus
    case "MinusAssign" => MinusAssign
    case "Decrement" => Decrement
    case "Star" => Star
    case "StarAssign" => StarAssign
    case "Modulo" => Modulo
    case "ModuloAssign" => ModuloAssign
    case "GreaterThan" => GreaterThan
    case "GreaterEqual" => GreaterEqual
    case "ShiftRight" => ShiftRight
    case "ShiftRightAssign" => ShiftRightAssign
    case "BinaryShiftRight" => BinaryShiftRight
    case "BinaryShiftRightAssign" => BinaryShiftRightAssign
    case "LessThan" => LessThan
    case "LessEqual" => LessEqual
    case "ShiftLeft" => ShiftLeft
    case "ShiftLeftAssign" => ShiftLeftAssign
    case "BinaryXor" => BinaryXor
    case "BinaryXorAssign" => BinaryXorAssign
    case "BinaryOr" => BinaryOr
    case "BinaryOrAssign" => BinaryOrAssign
    case "LogicalOr" => LogicalOr
    case "BinaryAnd" => BinaryAnd
    case "BinaryAndAssign" => BinaryAndAssign
    case "LogicalAnd" => LogicalAnd
    case "Semicolon" => Semicolon
    case "Whitespace" => Whitespace
    case "SingleLineComment" => SingleLineComment
    case "MultiLineComment" => MultiLineComment
    case "JavaDocComment" => JavaDocComment
    case "Num" => Num
    case "CharLiteral" => CharLiteral
    case "StringLiteral" => StringLiteral
    case "Identifier" => Identifier
    case "AbstractKeyword" => AbstractKeyword
    case "BooleanKeyword" => BooleanKeyword
    case "BreakKeyword" => BreakKeyword
    case "ByteKeyword" => ByteKeyword
    case "CaseKeyword" => CaseKeyword
    case "CatchKeyword" => CatchKeyword
    case "CharKeyword" => CharKeyword
    case "ClassKeyword" => ClassKeyword
    case "ConstKeyword" => ConstKeyword
    case "ContinueKeyword" => ContinueKeyword
    case "DefaultKeyword" => DefaultKeyword
    case "DoKeyword" => DoKeyword
    case "DoubleKeyword" => DoubleKeyword
    case "ElseKeyword" => ElseKeyword
    case "ExtendsKeyword" => ExtendsKeyword
    case "FinalKeyword" => FinalKeyword
    case "FinallyKeyword" => FinallyKeyword
    case "FloatKeyword" => FloatKeyword
    case "ForKeyword" => ForKeyword
    case "GotoKeyword" => GotoKeyword
    case "IfKeyword" => IfKeyword
    case "ImplementsKeyword" => ImplementsKeyword
    case "ImportKeyword" => ImportKeyword
    case "InstanceofKeyword" => InstanceofKeyword
    case "IntKeyword" => IntKeyword
    case "InterfaceKeyword" => InterfaceKeyword
    case "LongKeyword" => LongKeyword
    case "NativeKeyword" => NativeKeyword
    case "NewKeyword" => NewKeyword
    case "PackageKeyword" => PackageKeyword
    case "PrivateKeyword" => PrivateKeyword
    case "ProtectedKeyword" => ProtectedKeyword
    case "PublicKeyword" => PublicKeyword
    case "ReturnKeyword" => ReturnKeyword
    case "ShortKeyword" => ShortKeyword
    case "StaticKeyword" => StaticKeyword
    case "StrictfpKeyword" => StrictfpKeyword
    case "SuperKeyword" => SuperKeyword
    case "SwitchKeyword" => SwitchKeyword
    case "SynchronizedKeyword" => SynchronizedKeyword
    case "ThisKeyword" => ThisKeyword
    case "ThrowKeyword" => ThrowKeyword
    case "ThrowsKeyword" => ThrowsKeyword
    case "TransientKeyword" => TransientKeyword
    case "TryKeyword" => TryKeyword
    case "VoidKeyword" => VoidKeyword
    case "VolatileKeyword" => VolatileKeyword
    case "WhileKeyword" => WhileKeyword
    case "TrueLiteral" => TrueLiteral
    case "FalseLiteral" => FalseLiteral
    case "NullLiteral" => NullLiteral
  }
}



object Tokens {
  //  Classes of token objects themselves - these are instantiated.

  case class BOF(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BOF
  }

  case class EOF(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.EOF
  }

  case class S(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.S
  }

  case class Question(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Question
  }

  case class LeftParen(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LeftParen
  }

  case class RightParen(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.RightParen
  }

  case class LeftBracket(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LeftBracket
  }

  case class RightBracket(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.RightBracket
  }

  case class LeftCurly(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LeftCurly
  }

  case class RightCurly(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.RightCurly
  }

  case class Colon(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Colon
  }

  case class Comma(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Comma
  }

  case class Dot(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Dot
  }

  case class Assign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Assign
  }

  case class Equal(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Equal
  }

  case class LogicalNot(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LogicalNot
  }

  case class BinaryNot(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryNot
  }

  case class NotEqual(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.NotEqual
  }

  case class Divide(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Divide
  }

  case class DivideAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.DivideAssign
  }

  case class Plus(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Plus
  }

  case class PlusAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.PlusAssign
  }

  case class Increment(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Increment
  }

  case class Minus(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Minus
  }

  case class MinusAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.MinusAssign
  }

  case class Decrement(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Decrement
  }

  case class Star(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Star
  }

  case class StarAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.StarAssign
  }

  case class Modulo(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Modulo
  }

  case class ModuloAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.ModuloAssign
  }

  case class GreaterThan(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.GreaterThan
  }

  case class GreaterEqual(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.GreaterEqual
  }

  case class ShiftRight(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.ShiftRight
  }

  case class ShiftRightAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.ShiftRightAssign
  }

  case class BinaryShiftRight(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryShiftRight
  }

  case class BinaryShiftRightAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryShiftRightAssign
  }

  case class LessThan(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LessThan
  }

  case class LessEqual(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LessEqual
  }

  case class ShiftLeft(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.ShiftLeft
  }

  case class ShiftLeftAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.ShiftLeftAssign
  }

  case class BinaryXor(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryXor
  }

  case class BinaryXorAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryXorAssign
  }

  case class BinaryOr(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryOr
  }

  case class BinaryOrAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryOrAssign
  }

  case class LogicalOr(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LogicalOr
  }

  case class BinaryAnd(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryAnd
  }

  case class BinaryAndAssign(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.BinaryAndAssign
  }

  case class LogicalAnd(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.LogicalAnd
  }

  case class Semicolon(override val data: String) extends FixedToken(data) {
    override def tokenType: TokenType = TokenTypes.Semicolon
  }

  case class Whitespace(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.Whitespace
  }

  case class SingleLineComment(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.SingleLineComment
  }

  case class MultiLineComment(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.MultiLineComment
  }

  case class JavaDocComment(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.JavaDocComment
  }

  case class Num(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.Num
  }

  case class CharLiteral(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.CharLiteral
  }

  case class StringLiteral(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.StringLiteral
  }

  case class Identifier(override val data: String) extends VariableToken(data) {
    override def tokenType: TokenType = TokenTypes.Identifier
  }

  case class AbstractKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.AbstractKeyword
  }

  case class BooleanKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.BooleanKeyword
  }

  case class BreakKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.BreakKeyword
  }

  case class ByteKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ByteKeyword
  }

  case class CaseKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.CaseKeyword
  }

  case class CatchKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.CatchKeyword
  }

  case class CharKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.CharKeyword
  }

  case class ClassKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ClassKeyword
  }

  case class ConstKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ConstKeyword
  }

  case class ContinueKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ContinueKeyword
  }

  case class DefaultKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.DefaultKeyword
  }

  case class DoKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.DoKeyword
  }

  case class DoubleKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.DoubleKeyword
  }

  case class ElseKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ElseKeyword
  }

  case class ExtendsKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ExtendsKeyword
  }

  case class FinalKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.FinalKeyword
  }

  case class FinallyKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.FinallyKeyword
  }

  case class FloatKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.FloatKeyword
  }

  case class ForKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ForKeyword
  }

  case class GotoKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.GotoKeyword
  }

  case class IfKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.IfKeyword
  }

  case class ImplementsKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ImplementsKeyword
  }

  case class ImportKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ImportKeyword
  }

  case class InstanceofKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.InstanceofKeyword
  }

  case class IntKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.IntKeyword
  }

  case class InterfaceKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.InterfaceKeyword
  }

  case class LongKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.LongKeyword
  }

  case class NativeKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.NativeKeyword
  }

  case class NewKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.NewKeyword
  }

  case class PackageKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.PackageKeyword
  }

  case class PrivateKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.PrivateKeyword
  }

  case class ProtectedKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ProtectedKeyword
  }

  case class PublicKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.PublicKeyword
  }

  case class ReturnKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ReturnKeyword
  }

  case class ShortKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ShortKeyword
  }

  case class StaticKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.StaticKeyword
  }

  case class StrictfpKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.StrictfpKeyword
  }

  case class SuperKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.SuperKeyword
  }

  case class SwitchKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.SwitchKeyword
  }

  case class SynchronizedKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.SynchronizedKeyword
  }

  case class ThisKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ThisKeyword
  }

  case class ThrowKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ThrowKeyword
  }

  case class ThrowsKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.ThrowsKeyword
  }

  case class TransientKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.TransientKeyword
  }

  case class TryKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.TryKeyword
  }

  case class VoidKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.VoidKeyword
  }

  case class VolatileKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.VolatileKeyword
  }

  case class WhileKeyword(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.WhileKeyword
  }

  case class TrueLiteral(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.TrueLiteral
  }

  case class FalseLiteral(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.FalseLiteral
  }

  case class NullLiteral(override val data: String) extends KeywordToken(data) {
    override def tokenType: TokenType = TokenTypes.NullLiteral
  }

}
