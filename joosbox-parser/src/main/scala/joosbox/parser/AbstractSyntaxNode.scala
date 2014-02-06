package joosbox.parser

import joosbox.lexer.InputString

sealed trait AbstractSyntaxNode {
  def children: List[AbstractSyntaxNode] = List.empty[AbstractSyntaxNode]
}

object AbstractSyntaxNode {
  case class CharLiteral(value: InputString) extends AbstractSyntaxNode
  case class StringLiteral(value: InputString) extends AbstractSyntaxNode
  case object NullLiteral extends AbstractSyntaxNode
  case class Num(value: InputString) extends AbstractSyntaxNode

  case object TrueLiteral extends AbstractSyntaxNode
  case object FalseLiteral extends AbstractSyntaxNode

  case object BooleanKeyword extends AbstractSyntaxNode
  case object ByteKeyword extends AbstractSyntaxNode
  case object ShortKeyword extends AbstractSyntaxNode
  case object IntKeyword extends AbstractSyntaxNode
  case object CharKeyword extends AbstractSyntaxNode

  case class CompilationUnit(
    val packageDeclaration: Option[PackageDeclaration] = None,
    val importDeclarations: Seq[ImportDeclaration] = Seq.empty[ImportDeclaration],
    val typeDeclarations: Seq[TypeDeclaration] = Seq.empty[TypeDeclaration]
  ) extends AbstractSyntaxNode

  case class PackageDeclaration(name: InputString) extends AbstractSyntaxNode

  sealed trait ImportDeclaration extends AbstractSyntaxNode {
    def name: InputString
  }

  case class SingleTypeImportDeclaration(name: InputString) extends ImportDeclaration
  case class TypeImportOnDemandDeclaration(name: InputString) extends ImportDeclaration

  case class Identifier(value: InputString) extends AbstractSyntaxNode

  //TODO
  case class ClassBody() extends AbstractSyntaxNode
  case class InterfaceBody() extends AbstractSyntaxNode

  abstract class Name(val value: InputString) extends AbstractSyntaxNode 
  case class SimpleName(override val value: InputString) extends Name(value)
  case class QualifiedName(override val value: InputString) extends Name(value)

  abstract class ClassOrInterfaceType(override val value: InputString) extends Name(value)
  case class ClassType(override val value: InputString) extends ClassOrInterfaceType(value)
  case class InterfaceType(override val value: InputString) extends ClassOrInterfaceType(value)

  abstract class Modifier extends AbstractSyntaxNode

  abstract class AccessModifier extends Modifier
  abstract class NonAccessModifier extends Modifier
  case object StaticKeyword extends NonAccessModifier

  case object PublicKeyword extends AccessModifier
  case object ProtectedKeyword extends AccessModifier

  case object AbstractKeyword extends NonAccessModifier
  case object FinalKeyword extends NonAccessModifier
  case object NativeKeyword extends NonAccessModifier

  abstract class TypeDeclaration(
    val name: InputString,
    val modifiers: Seq[Modifier] = Seq.empty[Modifier],
    val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends AbstractSyntaxNode

  case class ClassDeclaration(
    override val name: InputString,
    val body: ClassBody,

    override val modifiers: Seq[Modifier] = Seq.empty[Modifier],
    val superclass: Option[ClassType] = None,
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces)

  case class InterfaceDeclaration(
    override val name: InputString,
    val body: InterfaceBody,

    override val modifiers: Seq[Modifier] = Seq.empty[Modifier],
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces)

  /*
  case class Star(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Star)
  }
  case class StaticKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.StaticKeyword)
  }
  case class PublicKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.PublicKeyword)
  }
  case class ProtectedKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ProtectedKeyword)
  }
  case class AbstractKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.AbstractKeyword)
  }
  case class FinalKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.FinalKeyword)
  }
  case class NativeKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NativeKeyword)
  }
  case class ClassKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ClassKeyword)
  }
  case class ExtendsKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ExtendsKeyword)
  }
  case class ImplementsKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ImplementsKeyword)
  }
  case class Comma(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Comma)
  }
  case class LeftCurly(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftCurly)
  }
  case class RightCurly(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightCurly)
  }
  case class Assign(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Assign)
  }
  case class LeftBracket(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftBracket)
  }
  case class RightBracket(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightBracket)
  }
  case class VoidKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.VoidKeyword)
  }
  case class LeftParen(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LeftParen)
  }
  case class RightParen(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.RightParen)
  }
  case class InterfaceKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.InterfaceKeyword)
  }
  case class IfKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.IfKeyword)
  }
  case class ElseKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ElseKeyword)
  }
  case class WhileKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.WhileKeyword)
  }
  case class ForKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ForKeyword)
  }
  case class ReturnKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ReturnKeyword)
  }
  case class ThisKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.ThisKeyword)
  }
  case class NewKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NewKeyword)
  }
  case class Plus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Plus)
  }
  case class Minus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Minus)
  }
  case class LogicalNot(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalNot)
  }
  case class Divide(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Divide)
  }
  case class Modulo(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Modulo)
  }
  case class LessThan(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LessThan)
  }
  case class GreaterThan(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.GreaterThan)
  }
  case class LessEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LessEqual)
  }
  case class GreaterEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.GreaterEqual)
  }
  case class InstanceofKeyword(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.InstanceofKeyword)
  }
  case class Equal(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.Equal)
  }
  case class NotEqual(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.NotEqual)
  }
  case class BinaryAnd(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryAnd)
  }
  case class BinaryXor(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryXor)
  }
  case class BinaryOr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.BinaryOr)
  }
  case class LogicalAnd(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalAnd)
  }
  case class LogicalOr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.LogicalOr)
  }
  case class S(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = Some(TokenTypes.S)
  }
  case class Literal(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IntegerLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BooleanLiteral(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Type(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PrimitiveType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class NumericType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ReferenceType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }

  case class ArrayType(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }

  case class ClassDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Super(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Interfaces(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceTypeList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBodyDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassBodyDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassMemberDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FieldDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclarators(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class VariableDeclaratorId(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodHeader(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FormalParameterList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FormalParameter(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorDeclarator(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstructorBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExtendsInterfaces(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceBody(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceMemberDeclarations(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InterfaceMemberDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConstantDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AbstractMethodDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Block(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BlockStatements(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class BlockStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LocalVariableDeclarationStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LocalVariableDeclaration(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Statement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementWithoutTrailingSubstatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class EmptyStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExpressionStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class StatementExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenElseStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class IfThenElseStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class WhileStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class WhileStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForStatementNoShortIf(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForInit(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ForUpdate(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ReturnStatement(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Primary(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PrimaryNoNewArray(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ClassInstanceCreationExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArgumentList(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArrayCreationExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class DimExprs(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class DimExpr(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Dims(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class FieldAccess(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MethodInvocation(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ArrayAccess(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class PostfixExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class UnaryExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class UnaryExpressionNotPlusMinus(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class CastExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class MultiplicativeExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AdditiveExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class RelationalExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class EqualityExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AndExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ExclusiveOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class InclusiveOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalAndExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalOrExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class ConditionalExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class AssignmentExpression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Assignment(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class LeftHandSide(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }
  case class Expression(override val children: List[ParseNode] = List.empty[ParseNode], override val value: Option[InputString] = None) extends ParseNode {
    def tokenType: Option[TokenType] = None
  }*/

  def fromParseNode(node: ParseNode): Seq[AbstractSyntaxNode] = node match {
    case s: ParseNodes.S    => fromParseNode(s.children(1))    //  Grab the compilation unit.
    case p: ParseNodes.BOF  => Seq.empty[AbstractSyntaxNode]
    case p: ParseNodes.EOF  => Seq.empty[AbstractSyntaxNode]

    case c: ParseNodes.CompilationUnit => {
      //  Sort our children into ImportDeclarations, TypeDeclarations and PackageDeclaration

      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      //  The grammar guarantees we should only get one PackageDeclaration,
      //  so there's no need to check for multiple here.
      val packageDeclaration: Option[PackageDeclaration] = children.flatMap {
        case x: PackageDeclaration => Some(x)
        case _ => None
      }.headOption

      val importDeclarations: Seq[ImportDeclaration] = children.flatMap {
        case x: ImportDeclaration => Some(x)
        case _ => None
      }

      val typeDeclarations: Seq[TypeDeclaration] = children.flatMap {
        case x: TypeDeclaration => Some(x)
        case _ => None
      }

      Seq(CompilationUnit(packageDeclaration, importDeclarations, typeDeclarations))
    }

    case p: ParseNodes.PackageDeclaration => Seq(PackageDeclaration(p.children(1).value.get))

    case i: ParseNodes.SingleTypeImportDeclaration   => Seq(SingleTypeImportDeclaration(i.children(1).value.get))
    case i: ParseNodes.TypeImportOnDemandDeclaration => Seq(TypeImportOnDemandDeclaration(i.children(1).value.get))

    case c: ParseNodes.ClassDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))
      
      val name:Identifier = children.flatMap {
        case x:Identifier => Some(x)
        case _ => None
      }.head

      val modifiers:Seq[Modifier] = children.flatMap {
        case x:Modifier => Some(x)
        case _ => None
      }

      val superclass:Option[ClassType] = children.flatMap {
        case x:ClassType => Some(x)
        case _ => None
      }.headOption

      val interfaces:Seq[InterfaceType] = children.flatMap {
        case x:InterfaceType => Some(x)
        case _ => None
      }

      val body:ClassBody = children.flatMap {
        case x:ClassBody => Some(x)
        case _ => None
      }.head

      Seq(ClassDeclaration(name.value, body, modifiers, superclass, interfaces))
    }

    case c: ParseNodes.InterfaceDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      val name:Identifier = children.flatMap {
        case x:Identifier => Some(x)
        case _ => None
      }.head

      val modifiers:Seq[Modifier] = children.flatMap {
        case x:Modifier => Some(x)
        case _ => None
      }

      val interfaces:Seq[InterfaceType] = children.flatMap {
        case x:InterfaceType => Some(x)
        case _ => None
      }

      val body:InterfaceBody = children.flatMap {
        case x:InterfaceBody => Some(x)
        case _ => None
      }.head

      Seq(InterfaceDeclaration(name.value, body, modifiers, interfaces))
    }

    case i: ParseNodes.Identifier => Seq(Identifier(i.value.get))

    //  TODO: Temp
    case i: ParseNodes.ClassBody => Seq(ClassBody())

    case m: ParseNodes.StaticKeyword => Seq(StaticKeyword)
    case m: ParseNodes.PublicKeyword => Seq(PublicKeyword)
    case m: ParseNodes.ProtectedKeyword => Seq(ProtectedKeyword)
    case m: ParseNodes.AbstractKeyword => Seq(AbstractKeyword)
    case m: ParseNodes.FinalKeyword => Seq(FinalKeyword)
    case m: ParseNodes.NativeKeyword => Seq(NativeKeyword)

    //  If the parse node does not map nicely to an ASN, just hand us its children.
    case p: ParseNode => p.children.flatMap(fromParseNode(_))
  }
}
