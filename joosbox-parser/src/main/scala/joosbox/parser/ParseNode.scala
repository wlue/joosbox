package joosbox.parser
import joosbox.lexer.Token
import joosbox.lexer.TokenType
import joosbox.lexer.TokenTypes

object ParseNodeTypes {

  object BOF extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BOF(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BOF)
  }

  object EOF extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.EOF(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.EOF)
  }

  object CharLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.CharLiteral(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.CharLiteral)
  }

  object StringLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.StringLiteral(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.StringLiteral)
  }

  object NullLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NullLiteral(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.NullLiteral)
  }

  object Num extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Num(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Num)
  }

  object TrueLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.TrueLiteral(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.TrueLiteral)
  }

  object FalseLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FalseLiteral(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.FalseLiteral)
  }

  object BooleanKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BooleanKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BooleanKeyword)
  }

  object ByteKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ByteKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ByteKeyword)
  }

  object ShortKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ShortKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ShortKeyword)
  }

  object IntKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IntKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.IntKeyword)
  }

  object CharKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.CharKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.CharKeyword)
  }

  object Identifier extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Identifier(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Identifier)
  }

  object Dot extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Dot(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Dot)
  }

  object PackageKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PackageKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.PackageKeyword)
  }

  object Semicolon extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Semicolon(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Semicolon)
  }

  object ImportKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ImportKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ImportKeyword)
  }

  object Star extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Star(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Star)
  }

  object StaticKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.StaticKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.StaticKeyword)
  }

  object PublicKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PublicKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.PublicKeyword)
  }

  object ProtectedKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ProtectedKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ProtectedKeyword)
  }

  object PrivateKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PrivateKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.PrivateKeyword)
  }

  object AbstractKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AbstractKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.AbstractKeyword)
  }

  object FinalKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FinalKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.FinalKeyword)
  }

  object NativeKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NativeKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.NativeKeyword)
  }

  object ClassKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ClassKeyword)
  }

  object ExtendsKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ExtendsKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ExtendsKeyword)
  }

  object ImplementsKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ImplementsKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ImplementsKeyword)
  }

  object Comma extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Comma(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Comma)
  }

  object LeftCurly extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LeftCurly(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LeftCurly)
  }

  object RightCurly extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.RightCurly(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.RightCurly)
  }

  object Assign extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Assign(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Assign)
  }

  object LeftBracket extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LeftBracket(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LeftBracket)
  }

  object RightBracket extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.RightBracket(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.RightBracket)
  }

  object VoidKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.VoidKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.VoidKeyword)
  }

  object LeftParen extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LeftParen(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LeftParen)
  }

  object RightParen extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.RightParen(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.RightParen)
  }

  object InterfaceKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.InterfaceKeyword)
  }

  object IfKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IfKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.IfKeyword)
  }

  object ElseKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ElseKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ElseKeyword)
  }

  object WhileKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.WhileKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.WhileKeyword)
  }

  object ForKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ForKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ForKeyword)
  }

  object ReturnKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ReturnKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ReturnKeyword)
  }

  object ThisKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ThisKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.ThisKeyword)
  }

  object NewKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NewKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.NewKeyword)
  }

  object Plus extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Plus(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Plus)
  }

  object Minus extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Minus(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Minus)
  }

  object BinaryNot extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BinaryNot(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BinaryNot)
  }

  object LogicalNot extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LogicalNot(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LogicalNot)
  }

  object Divide extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Divide(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Divide)
  }

  object Modulo extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Modulo(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Modulo)
  }

  object LessThan extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LessThan(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LessThan)
  }

  object GreaterThan extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.GreaterThan(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.GreaterThan)
  }

  object LessEqual extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LessEqual(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LessEqual)
  }

  object GreaterEqual extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.GreaterEqual(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.GreaterEqual)
  }

  object InstanceofKeyword extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InstanceofKeyword(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.InstanceofKeyword)
  }

  object Equal extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Equal(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.Equal)
  }

  object NotEqual extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NotEqual(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.NotEqual)
  }

  object BinaryAnd extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BinaryAnd(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BinaryAnd)
  }

  object BinaryXor extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BinaryXor(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BinaryXor)
  }

  object BinaryOr extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BinaryOr(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.BinaryOr)
  }

  object LogicalAnd extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LogicalAnd(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LogicalAnd)
  }

  object LogicalOr extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LogicalOr(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.LogicalOr)
  }

  object S extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.S(children, value)
    override def tokenType: Option[TokenType] = Some(TokenTypes.S)
  }

  object Literal extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Literal(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object IntegerLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IntegerLiteral(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object BooleanLiteral extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BooleanLiteral(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Type extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Type(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object PrimitiveType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PrimitiveType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object NumericType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NumericType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ReferenceType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ReferenceType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassOrInterfaceType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassOrInterfaceType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ArrayType extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ArrayType(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Name extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Name(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object SimpleName extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.SimpleName(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object QualifiedName extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.QualifiedName(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object CompilationUnit extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.CompilationUnit(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ImportDeclarations extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ImportDeclarations(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object TypeDeclarations extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.TypeDeclarations(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object PackageDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PackageDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ImportDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ImportDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object SingleTypeImportDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.SingleTypeImportDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object TypeImportOnDemandDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.TypeImportOnDemandDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object TypeDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.TypeDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Modifiers extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Modifiers(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Modifier extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Modifier(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object AccessModifier extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AccessModifier(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object NonAccessModifier extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.NonAccessModifier(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Super extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Super(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Interfaces extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Interfaces(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceTypeList extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceTypeList(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassBody extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassBody(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassBodyDeclarations extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassBodyDeclarations(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassBodyDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassBodyDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassMemberDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassMemberDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object FieldDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FieldDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object VariableDeclarators extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.VariableDeclarators(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object VariableDeclarator extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.VariableDeclarator(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object VariableDeclaratorId extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.VariableDeclaratorId(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MethodDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MethodDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MethodHeader extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MethodHeader(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MethodDeclarator extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MethodDeclarator(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object FormalParameterList extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FormalParameterList(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object FormalParameter extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FormalParameter(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MethodBody extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MethodBody(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConstructorDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConstructorDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConstructorDeclarator extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConstructorDeclarator(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConstructorBody extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConstructorBody(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ExtendsInterfaces extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ExtendsInterfaces(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceBody extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceBody(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceMemberDeclarations extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceMemberDeclarations(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InterfaceMemberDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InterfaceMemberDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConstantDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConstantDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object AbstractMethodDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AbstractMethodDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Block extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Block(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object BlockStatements extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BlockStatements(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object BlockStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.BlockStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object LocalVariableDeclarationStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LocalVariableDeclarationStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object LocalVariableDeclaration extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LocalVariableDeclaration(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Statement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Statement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object StatementNoShortIf extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.StatementNoShortIf(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object StatementWithoutTrailingSubstatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.StatementWithoutTrailingSubstatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object EmptyStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.EmptyStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ExpressionStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ExpressionStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object StatementExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.StatementExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object IfThenStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IfThenStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object IfThenElseStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IfThenElseStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object IfThenElseStatementNoShortIf extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.IfThenElseStatementNoShortIf(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object WhileStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.WhileStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object WhileStatementNoShortIf extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.WhileStatementNoShortIf(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ForStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ForStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ForStatementNoShortIf extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ForStatementNoShortIf(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ForInit extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ForInit(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ForUpdate extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ForUpdate(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ReturnStatement extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ReturnStatement(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Primary extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Primary(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object PrimaryNoNewArray extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PrimaryNoNewArray(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ClassInstanceCreationExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ClassInstanceCreationExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ArgumentList extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ArgumentList(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ArrayCreationExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ArrayCreationExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object DimExprs extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.DimExprs(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object DimExpr extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.DimExpr(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Dims extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Dims(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object FieldAccess extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.FieldAccess(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MethodInvocation extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MethodInvocation(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ArrayAccess extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ArrayAccess(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object PostfixExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.PostfixExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object UnaryExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.UnaryExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object UnaryExpressionNotPlusMinus extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.UnaryExpressionNotPlusMinus(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object CastExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.CastExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object MultiplicativeExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.MultiplicativeExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object AdditiveExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AdditiveExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object RelationalExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.RelationalExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object EqualityExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.EqualityExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object AndExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AndExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ExclusiveOrExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ExclusiveOrExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object InclusiveOrExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.InclusiveOrExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConditionalAndExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConditionalAndExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConditionalOrExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConditionalOrExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object ConditionalExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.ConditionalExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object AssignmentExpression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.AssignmentExpression(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Assignment extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Assignment(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object LeftHandSide extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.LeftHandSide(children, value)
    override def tokenType: Option[TokenType] = None
  }

  object Expression extends ParseNodeType {
    override def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode =
      new ParseNodes.Expression(children, value)
    override def tokenType: Option[TokenType] = None
  }
  def fromString(input: String): ParseNodeType = input match {
    case "BOF" => BOF
    case "EOF" => EOF
    case "CharLiteral" => CharLiteral
    case "StringLiteral" => StringLiteral
    case "NullLiteral" => NullLiteral
    case "Num" => Num
    case "TrueLiteral" => TrueLiteral
    case "FalseLiteral" => FalseLiteral
    case "BooleanKeyword" => BooleanKeyword
    case "ByteKeyword" => ByteKeyword
    case "ShortKeyword" => ShortKeyword
    case "IntKeyword" => IntKeyword
    case "CharKeyword" => CharKeyword
    case "Identifier" => Identifier
    case "Dot" => Dot
    case "PackageKeyword" => PackageKeyword
    case "Semicolon" => Semicolon
    case "ImportKeyword" => ImportKeyword
    case "Star" => Star
    case "StaticKeyword" => StaticKeyword
    case "PublicKeyword" => PublicKeyword
    case "ProtectedKeyword" => ProtectedKeyword
    case "PrivateKeyword" => PrivateKeyword
    case "AbstractKeyword" => AbstractKeyword
    case "FinalKeyword" => FinalKeyword
    case "NativeKeyword" => NativeKeyword
    case "ClassKeyword" => ClassKeyword
    case "ExtendsKeyword" => ExtendsKeyword
    case "ImplementsKeyword" => ImplementsKeyword
    case "Comma" => Comma
    case "LeftCurly" => LeftCurly
    case "RightCurly" => RightCurly
    case "Assign" => Assign
    case "LeftBracket" => LeftBracket
    case "RightBracket" => RightBracket
    case "VoidKeyword" => VoidKeyword
    case "LeftParen" => LeftParen
    case "RightParen" => RightParen
    case "InterfaceKeyword" => InterfaceKeyword
    case "IfKeyword" => IfKeyword
    case "ElseKeyword" => ElseKeyword
    case "WhileKeyword" => WhileKeyword
    case "ForKeyword" => ForKeyword
    case "ReturnKeyword" => ReturnKeyword
    case "ThisKeyword" => ThisKeyword
    case "NewKeyword" => NewKeyword
    case "Plus" => Plus
    case "Minus" => Minus
    case "BinaryNot" => BinaryNot
    case "LogicalNot" => LogicalNot
    case "Divide" => Divide
    case "Modulo" => Modulo
    case "LessThan" => LessThan
    case "GreaterThan" => GreaterThan
    case "LessEqual" => LessEqual
    case "GreaterEqual" => GreaterEqual
    case "InstanceofKeyword" => InstanceofKeyword
    case "Equal" => Equal
    case "NotEqual" => NotEqual
    case "BinaryAnd" => BinaryAnd
    case "BinaryXor" => BinaryXor
    case "BinaryOr" => BinaryOr
    case "LogicalAnd" => LogicalAnd
    case "LogicalOr" => LogicalOr
    case "S" => S
    case "Literal" => Literal
    case "IntegerLiteral" => IntegerLiteral
    case "BooleanLiteral" => BooleanLiteral
    case "Type" => Type
    case "PrimitiveType" => PrimitiveType
    case "NumericType" => NumericType
    case "ReferenceType" => ReferenceType
    case "ClassOrInterfaceType" => ClassOrInterfaceType
    case "ClassType" => ClassType
    case "InterfaceType" => InterfaceType
    case "ArrayType" => ArrayType
    case "Name" => Name
    case "SimpleName" => SimpleName
    case "QualifiedName" => QualifiedName
    case "CompilationUnit" => CompilationUnit
    case "ImportDeclarations" => ImportDeclarations
    case "TypeDeclarations" => TypeDeclarations
    case "PackageDeclaration" => PackageDeclaration
    case "ImportDeclaration" => ImportDeclaration
    case "SingleTypeImportDeclaration" => SingleTypeImportDeclaration
    case "TypeImportOnDemandDeclaration" => TypeImportOnDemandDeclaration
    case "TypeDeclaration" => TypeDeclaration
    case "Modifiers" => Modifiers
    case "Modifier" => Modifier
    case "AccessModifier" => AccessModifier
    case "NonAccessModifier" => NonAccessModifier
    case "ClassDeclaration" => ClassDeclaration
    case "Super" => Super
    case "Interfaces" => Interfaces
    case "InterfaceTypeList" => InterfaceTypeList
    case "ClassBody" => ClassBody
    case "ClassBodyDeclarations" => ClassBodyDeclarations
    case "ClassBodyDeclaration" => ClassBodyDeclaration
    case "ClassMemberDeclaration" => ClassMemberDeclaration
    case "FieldDeclaration" => FieldDeclaration
    case "VariableDeclarators" => VariableDeclarators
    case "VariableDeclarator" => VariableDeclarator
    case "VariableDeclaratorId" => VariableDeclaratorId
    case "MethodDeclaration" => MethodDeclaration
    case "MethodHeader" => MethodHeader
    case "MethodDeclarator" => MethodDeclarator
    case "FormalParameterList" => FormalParameterList
    case "FormalParameter" => FormalParameter
    case "MethodBody" => MethodBody
    case "ConstructorDeclaration" => ConstructorDeclaration
    case "ConstructorDeclarator" => ConstructorDeclarator
    case "ConstructorBody" => ConstructorBody
    case "InterfaceDeclaration" => InterfaceDeclaration
    case "ExtendsInterfaces" => ExtendsInterfaces
    case "InterfaceBody" => InterfaceBody
    case "InterfaceMemberDeclarations" => InterfaceMemberDeclarations
    case "InterfaceMemberDeclaration" => InterfaceMemberDeclaration
    case "ConstantDeclaration" => ConstantDeclaration
    case "AbstractMethodDeclaration" => AbstractMethodDeclaration
    case "Block" => Block
    case "BlockStatements" => BlockStatements
    case "BlockStatement" => BlockStatement
    case "LocalVariableDeclarationStatement" => LocalVariableDeclarationStatement
    case "LocalVariableDeclaration" => LocalVariableDeclaration
    case "Statement" => Statement
    case "StatementNoShortIf" => StatementNoShortIf
    case "StatementWithoutTrailingSubstatement" => StatementWithoutTrailingSubstatement
    case "EmptyStatement" => EmptyStatement
    case "ExpressionStatement" => ExpressionStatement
    case "StatementExpression" => StatementExpression
    case "IfThenStatement" => IfThenStatement
    case "IfThenElseStatement" => IfThenElseStatement
    case "IfThenElseStatementNoShortIf" => IfThenElseStatementNoShortIf
    case "WhileStatement" => WhileStatement
    case "WhileStatementNoShortIf" => WhileStatementNoShortIf
    case "ForStatement" => ForStatement
    case "ForStatementNoShortIf" => ForStatementNoShortIf
    case "ForInit" => ForInit
    case "ForUpdate" => ForUpdate
    case "ReturnStatement" => ReturnStatement
    case "Primary" => Primary
    case "PrimaryNoNewArray" => PrimaryNoNewArray
    case "ClassInstanceCreationExpression" => ClassInstanceCreationExpression
    case "ArgumentList" => ArgumentList
    case "ArrayCreationExpression" => ArrayCreationExpression
    case "DimExprs" => DimExprs
    case "DimExpr" => DimExpr
    case "Dims" => Dims
    case "FieldAccess" => FieldAccess
    case "MethodInvocation" => MethodInvocation
    case "ArrayAccess" => ArrayAccess
    case "PostfixExpression" => PostfixExpression
    case "UnaryExpression" => UnaryExpression
    case "UnaryExpressionNotPlusMinus" => UnaryExpressionNotPlusMinus
    case "CastExpression" => CastExpression
    case "MultiplicativeExpression" => MultiplicativeExpression
    case "AdditiveExpression" => AdditiveExpression
    case "RelationalExpression" => RelationalExpression
    case "EqualityExpression" => EqualityExpression
    case "AndExpression" => AndExpression
    case "ExclusiveOrExpression" => ExclusiveOrExpression
    case "InclusiveOrExpression" => InclusiveOrExpression
    case "ConditionalAndExpression" => ConditionalAndExpression
    case "ConditionalOrExpression" => ConditionalOrExpression
    case "ConditionalExpression" => ConditionalExpression
    case "AssignmentExpression" => AssignmentExpression
    case "Assignment" => Assignment
    case "LeftHandSide" => LeftHandSide
    case "Expression" => Expression
  }

  def fromTokenType(input: TokenType) : ParseNodeType = input match {
    case TokenTypes.BOF => BOF
    case TokenTypes.EOF => EOF
    case TokenTypes.CharLiteral => CharLiteral
    case TokenTypes.StringLiteral => StringLiteral
    case TokenTypes.NullLiteral => NullLiteral
    case TokenTypes.Num => Num
    case TokenTypes.TrueLiteral => TrueLiteral
    case TokenTypes.FalseLiteral => FalseLiteral
    case TokenTypes.BooleanKeyword => BooleanKeyword
    case TokenTypes.ByteKeyword => ByteKeyword
    case TokenTypes.ShortKeyword => ShortKeyword
    case TokenTypes.IntKeyword => IntKeyword
    case TokenTypes.CharKeyword => CharKeyword
    case TokenTypes.Identifier => Identifier
    case TokenTypes.Dot => Dot
    case TokenTypes.PackageKeyword => PackageKeyword
    case TokenTypes.Semicolon => Semicolon
    case TokenTypes.ImportKeyword => ImportKeyword
    case TokenTypes.Star => Star
    case TokenTypes.StaticKeyword => StaticKeyword
    case TokenTypes.PublicKeyword => PublicKeyword
    case TokenTypes.ProtectedKeyword => ProtectedKeyword
    case TokenTypes.PrivateKeyword => PrivateKeyword
    case TokenTypes.AbstractKeyword => AbstractKeyword
    case TokenTypes.FinalKeyword => FinalKeyword
    case TokenTypes.NativeKeyword => NativeKeyword
    case TokenTypes.ClassKeyword => ClassKeyword
    case TokenTypes.ExtendsKeyword => ExtendsKeyword
    case TokenTypes.ImplementsKeyword => ImplementsKeyword
    case TokenTypes.Comma => Comma
    case TokenTypes.LeftCurly => LeftCurly
    case TokenTypes.RightCurly => RightCurly
    case TokenTypes.Assign => Assign
    case TokenTypes.LeftBracket => LeftBracket
    case TokenTypes.RightBracket => RightBracket
    case TokenTypes.VoidKeyword => VoidKeyword
    case TokenTypes.LeftParen => LeftParen
    case TokenTypes.RightParen => RightParen
    case TokenTypes.InterfaceKeyword => InterfaceKeyword
    case TokenTypes.IfKeyword => IfKeyword
    case TokenTypes.ElseKeyword => ElseKeyword
    case TokenTypes.WhileKeyword => WhileKeyword
    case TokenTypes.ForKeyword => ForKeyword
    case TokenTypes.ReturnKeyword => ReturnKeyword
    case TokenTypes.ThisKeyword => ThisKeyword
    case TokenTypes.NewKeyword => NewKeyword
    case TokenTypes.Plus => Plus
    case TokenTypes.Minus => Minus
    case TokenTypes.BinaryNot => BinaryNot
    case TokenTypes.LogicalNot => LogicalNot
    case TokenTypes.Divide => Divide
    case TokenTypes.Modulo => Modulo
    case TokenTypes.LessThan => LessThan
    case TokenTypes.GreaterThan => GreaterThan
    case TokenTypes.LessEqual => LessEqual
    case TokenTypes.GreaterEqual => GreaterEqual
    case TokenTypes.InstanceofKeyword => InstanceofKeyword
    case TokenTypes.Equal => Equal
    case TokenTypes.NotEqual => NotEqual
    case TokenTypes.BinaryAnd => BinaryAnd
    case TokenTypes.BinaryXor => BinaryXor
    case TokenTypes.BinaryOr => BinaryOr
    case TokenTypes.LogicalAnd => LogicalAnd
    case TokenTypes.LogicalOr => LogicalOr
    case TokenTypes.S => S
  }
}

object ParseNodes {
  def fromToken(input: Token) : ParseNode = ParseNodeTypes.fromTokenType(input.tokenType)(List.empty[ParseNode], if (input.data == "") None else Some(input.data))

  case class BOF(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BOF)
  }
  case class EOF(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.EOF)
  }
  case class CharLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.CharLiteral)
  }
  case class StringLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.StringLiteral)
  }
  case class NullLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NullLiteral)
  }
  case class Num(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Num)
  }
  case class TrueLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.TrueLiteral)
  }
  case class FalseLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.FalseLiteral)
  }
  case class BooleanKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BooleanKeyword)
  }
  case class ByteKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ByteKeyword)
  }
  case class ShortKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ShortKeyword)
  }
  case class IntKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.IntKeyword)
  }
  case class CharKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.CharKeyword)
  }
  case class Identifier(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Identifier)
  }
  case class Dot(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Dot)
  }
  case class PackageKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.PackageKeyword)
  }
  case class Semicolon(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Semicolon)
  }
  case class ImportKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ImportKeyword)
  }
  case class Star(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Star)
  }
  case class StaticKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.StaticKeyword)
  }
  case class PublicKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.PublicKeyword)
  }
  case class ProtectedKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ProtectedKeyword)
  }
  case class PrivateKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.PrivateKeyword)
  }
  case class AbstractKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.AbstractKeyword)
  }
  case class FinalKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.FinalKeyword)
  }
  case class NativeKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NativeKeyword)
  }
  case class ClassKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ClassKeyword)
  }
  case class ExtendsKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ExtendsKeyword)
  }
  case class ImplementsKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ImplementsKeyword)
  }
  case class Comma(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Comma)
  }
  case class LeftCurly(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftCurly)
  }
  case class RightCurly(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightCurly)
  }
  case class Assign(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Assign)
  }
  case class LeftBracket(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftBracket)
  }
  case class RightBracket(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightBracket)
  }
  case class VoidKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.VoidKeyword)
  }
  case class LeftParen(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftParen)
  }
  case class RightParen(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightParen)
  }
  case class InterfaceKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.InterfaceKeyword)
  }
  case class IfKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.IfKeyword)
  }
  case class ElseKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ElseKeyword)
  }
  case class WhileKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.WhileKeyword)
  }
  case class ForKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ForKeyword)
  }
  case class ReturnKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ReturnKeyword)
  }
  case class ThisKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ThisKeyword)
  }
  case class NewKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NewKeyword)
  }
  case class Plus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Plus)
  }
  case class Minus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Minus)
  }
  case class BinaryNot(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryNot)
  }
  case class LogicalNot(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalNot)
  }
  case class Divide(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Divide)
  }
  case class Modulo(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Modulo)
  }
  case class LessThan(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LessThan)
  }
  case class GreaterThan(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.GreaterThan)
  }
  case class LessEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LessEqual)
  }
  case class GreaterEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.GreaterEqual)
  }
  case class InstanceofKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.InstanceofKeyword)
  }
  case class Equal(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Equal)
  }
  case class NotEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NotEqual)
  }
  case class BinaryAnd(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryAnd)
  }
  case class BinaryXor(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryXor)
  }
  case class BinaryOr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryOr)
  }
  case class LogicalAnd(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalAnd)
  }
  case class LogicalOr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalOr)
  }
  case class S(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.S)
  }
  case class Literal(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IntegerLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BooleanLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Type(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PrimitiveType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class NumericType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ReferenceType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassOrInterfaceType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArrayType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Name(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class SimpleName(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class QualifiedName(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class CompilationUnit(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ImportDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class TypeDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PackageDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ImportDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class SingleTypeImportDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class TypeImportOnDemandDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class TypeDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Modifiers(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Modifier(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AccessModifier(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class NonAccessModifier(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Super(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Interfaces(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceTypeList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBodyDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBodyDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassMemberDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FieldDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclarators(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclaratorId(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodHeader(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FormalParameterList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FormalParameter(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExtendsInterfaces(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceMemberDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceMemberDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstantDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AbstractMethodDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Block(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BlockStatements(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BlockStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LocalVariableDeclarationStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LocalVariableDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Statement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementWithoutTrailingSubstatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class EmptyStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExpressionStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenElseStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenElseStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class WhileStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class WhileStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForInit(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForUpdate(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ReturnStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Primary(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PrimaryNoNewArray(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassInstanceCreationExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArgumentList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArrayCreationExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class DimExprs(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class DimExpr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Dims(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FieldAccess(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodInvocation(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArrayAccess(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PostfixExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class UnaryExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class UnaryExpressionNotPlusMinus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class CastExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MultiplicativeExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AdditiveExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class RelationalExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class EqualityExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AndExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExclusiveOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InclusiveOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalAndExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AssignmentExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Assignment(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LeftHandSide(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Expression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[String] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
}
