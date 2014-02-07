package joosbox.parser

import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

sealed trait AbstractSyntaxNode {
  def children: List[AbstractSyntaxNode] = List.empty[AbstractSyntaxNode]
}

object AbstractSyntaxNode {
  case class CharLiteral(value: InputString) extends AbstractSyntaxNode
  case class StringLiteral(value: InputString) extends AbstractSyntaxNode
  case object NullLiteral extends AbstractSyntaxNode
  case class Num(value: InputString) extends AbstractSyntaxNode

  case class CompilationUnit(
    packageDeclaration: Option[PackageDeclaration] = None,
    importDeclarations: Seq[ImportDeclaration] = Seq.empty[ImportDeclaration],
    typeDeclarations: Seq[TypeDeclaration] = Seq.empty[TypeDeclaration]
  ) extends AbstractSyntaxNode

  case class PackageDeclaration(name: InputString) extends AbstractSyntaxNode

  sealed trait ImportDeclaration extends AbstractSyntaxNode {
    def name: InputString
  }

  case class SingleTypeImportDeclaration(name: InputString) extends ImportDeclaration
  case class TypeImportOnDemandDeclaration(name: InputString) extends ImportDeclaration

  case class Identifier(value: InputString) extends AbstractSyntaxNode

  abstract class ClassBodyDeclaration(
    val name: InputString,
    val modifiers: Set[Modifier] = Set.empty[Modifier],
    val memberType: Type
  ) extends AbstractSyntaxNode

  //  TODO; implement me properly, not just stubbed out
  case class InterfaceMemberDeclaration() extends AbstractSyntaxNode

  case class ClassBody(val declarations: Seq[ClassBodyDeclaration] = Seq.empty[ClassBodyDeclaration]) extends AbstractSyntaxNode
  case class InterfaceBody(val declarations: Seq[InterfaceMemberDeclaration] = Seq.empty[InterfaceMemberDeclaration]) extends AbstractSyntaxNode
  case class MethodBody() extends AbstractSyntaxNode

  abstract class Expression() extends AbstractSyntaxNode

  //  TODO: There should be a type that wraps Type and inherits from it for ArrayOf
  sealed trait Type extends AbstractSyntaxNode

  sealed trait Name extends Type
  case class SimpleName(val value: InputString) extends Name
  case class QualifiedName(val value: Seq[InputString]) extends Name

  sealed trait PrimitiveType extends Type
  sealed trait ReferenceType extends Type
  case object VoidKeyword extends Type

  sealed trait BooleanKeyword extends PrimitiveType

  case object TrueLiteral extends BooleanKeyword
  case object FalseLiteral extends BooleanKeyword

  sealed trait NumericType extends PrimitiveType

  case object ByteKeyword extends NumericType
  case object ShortKeyword extends NumericType
  case object IntKeyword extends NumericType
  case object CharKeyword extends NumericType

  case class ArrayType(val subtype: Type) extends ReferenceType

  case class ClassOrInterfaceType(val name: Name) extends ReferenceType 
  case class ClassType(val name: Name) extends ReferenceType
  case class InterfaceType(val name: Name) extends ReferenceType

  abstract class FormalParameter(val name: InputString, val varType: Type) extends AbstractSyntaxNode

  sealed trait Modifier extends AbstractSyntaxNode

  sealed trait AccessModifier extends Modifier
  sealed trait NonAccessModifier extends Modifier
  case object StaticKeyword extends NonAccessModifier

  case object PublicKeyword extends AccessModifier
  case object ProtectedKeyword extends AccessModifier

  case object AbstractKeyword extends NonAccessModifier
  case object FinalKeyword extends NonAccessModifier
  case object NativeKeyword extends NonAccessModifier

  abstract class TypeDeclaration(
    val name: InputString,
    val modifiers: Set[Modifier] = Set.empty[Modifier],
    val interfaces: Set[InterfaceType] = Set.empty[InterfaceType]
  ) extends AbstractSyntaxNode

  case class ClassDeclaration(
    override val name: InputString,
    val body: ClassBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    val superclass: Option[ClassType] = None,
    override val interfaces: Set[InterfaceType] = Set.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces)

  case class InterfaceDeclaration(
    override val name: InputString,
    val body: InterfaceBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val interfaces: Set[InterfaceType] = Set.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces)

  abstract class ClassMemberDeclaration(
    override val name: InputString,
    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val memberType: Type
  ) extends ClassBodyDeclaration(name, modifiers, memberType)

  case class MethodDeclaration(
    override val name: InputString,
    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val memberType: Type,

    val parameters: Set[FormalParameter] = Set.empty[FormalParameter],
    val body: Option[MethodBody] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType)

  case class FieldDeclaration(
    override val name: InputString,
    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val memberType: Type,

    val expression: Option[Expression] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType)

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
      // Sort our children into ImportDeclarations, TypeDeclarations and PackageDeclaration
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      // The grammar guarantees we should only get one PackageDeclaration,
      // so there's no need to check for multiple here.
      val packageDeclaration: Option[PackageDeclaration] = children.collectFirst { case x: PackageDeclaration => x }
      val importDeclarations: Seq[ImportDeclaration] = children.collect { case x: ImportDeclaration => x }
      val typeDeclarations: Seq[TypeDeclaration] = children.collect { case x: TypeDeclaration => x }

      Seq(CompilationUnit(packageDeclaration, importDeclarations, typeDeclarations))
    }

    case p: ParseNodes.PackageDeclaration => Seq(PackageDeclaration(p.children(1).value.get))

    case i: ParseNodes.SingleTypeImportDeclaration   => Seq(SingleTypeImportDeclaration(i.children(1).value.get))
    case i: ParseNodes.TypeImportOnDemandDeclaration => Seq(TypeImportOnDemandDeclaration(i.children(1).value.get))

    case c: ParseNodes.ClassDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))
      val name:Identifier = children.collectFirst { case x: Identifier => x }.get

      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val superclass:Option[ClassType] = children.collectFirst { case x: ClassType => x }
      val interfaces:Set[InterfaceType] = children.collect { case x: InterfaceType => x }.toSet
      val body:ClassBody = children.collectFirst { case x: ClassBody => x }.get

      //  Enforce "A class cannot be both abstract and final."
      if (modifiers.contains(AbstractKeyword) && modifiers.contains(FinalKeyword)) {
        throw new SyntaxError("Class " + name.value + " cannot be both abstract and final.")
      }
      // Enforce: No package private classes
      if (!modifiers.contains(PublicKeyword) && !modifiers.contains(ProtectedKeyword)) {
        throw new SyntaxError("Class " + name.value + " cannot be package private.")
      }

      // Enforce: Class must match filename
      val filename = name.value.filename.split("/").last
      if (filename != name.value.value + ".java") {
        if (!filename.contains("<input>")) {
          throw new SyntaxError("Class " + name.value + " must be in a file with the same name.")
        }
      }

      Seq(ClassDeclaration(name.value, body, modifiers, superclass, interfaces))
    }

    case c: ParseNodes.InterfaceDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val interfaces:Set[InterfaceType] = children.collect { case x: InterfaceType => x }.toSet
      val body:InterfaceBody = children.collectFirst { case x: InterfaceBody => x }.get

      // Enforce: No package private interface
      if (!modifiers.contains(PublicKeyword) && !modifiers.contains(ProtectedKeyword)) {
        throw new SyntaxError("Interface " + name.value + " cannot be package private.")
      }

      // Enforce: Interface must match filename
      val filename = name.value.filename.split("/").last
      if (filename != name.value.value + ".java") {
        if (!filename.contains("<input>")) {
          throw new SyntaxError("Interface " + name.value + " must be in a file with the same name.")
        }
      }

      Seq(InterfaceDeclaration(name.value, body, modifiers, interfaces))
    }

    case c: ParseNodes.MethodDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType:Type = children.collectFirst { case x:Type => x }.get
      val parameters: Set[FormalParameter] = children.collect { case x:FormalParameter => x }.toSet
      val body:Option[MethodBody] = children.collectFirst { case x:MethodBody => x }

      // Enforce: No package private methods
      if (!modifiers.contains(PublicKeyword) && !modifiers.contains(ProtectedKeyword)) {
        throw new SyntaxError("Method " + name.value + " cannot be package private.")
      }

      // Enforce: A static method cannot be final.
      if (modifiers.contains(StaticKeyword) && modifiers.contains(FinalKeyword)) {
        throw new SyntaxError("Method " + name.value + " cannot be both static and final.")
      }

      // Enforce: A native method must be static.
      if (modifiers.contains(NativeKeyword) && !modifiers.contains(StaticKeyword)) {
        throw new SyntaxError("Method " + name.value + " is native so it must be static.")
      }

      //Enforce: An abstract method cannot be static or final.
      if (modifiers.contains(AbstractKeyword)) {
        if (modifiers.contains(StaticKeyword) || modifiers.contains(FinalKeyword)) {
          throw new SyntaxError("Method " + name.value + " is abstract so it cant be static or final.")
        }
      }

      // Enforce: A method has a body if and only if it is neither abstract nor native.
      if (modifiers.contains(NativeKeyword) || modifiers.contains(AbstractKeyword)) {
        if (!body.isEmpty) {
          throw new SyntaxError("Method " + name.value + " cannot have a body.")
        }
      } else {
        if (body.isEmpty) {
          throw new SyntaxError("Method " + name.value + " must have a body.")
        }
      }

      Seq(MethodDeclaration(name.value, modifiers, memberType, parameters, body))
    }

    case c: ParseNodes.FieldDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType:Type = children.collectFirst { case x:Type => x }.get
      val expression:Option[Expression] = children.collectFirst { case x:Expression => x }

      Seq(FieldDeclaration(name.value, modifiers, memberType, expression))
    }

    case i: ParseNodes.Identifier => Seq(Identifier(i.value.get))

    case c: ParseNodes.ClassBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))
      Seq(ClassBody(children.collect { case x: ClassBodyDeclaration => x }))
    }

    case c: ParseNodes.InterfaceBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(fromParseNode(_))
      Seq(InterfaceBody(children.collect { case x: InterfaceMemberDeclaration => x }))
    }

    //  TODO: IMPLEMENT ME
    case c: ParseNodes.InterfaceMemberDeclaration => {
      Seq(InterfaceMemberDeclaration())
    }

    case m: ParseNodes.StaticKeyword => Seq(StaticKeyword)
    case m: ParseNodes.PublicKeyword => Seq(PublicKeyword)
    case m: ParseNodes.ProtectedKeyword => Seq(ProtectedKeyword)
    case m: ParseNodes.AbstractKeyword => Seq(AbstractKeyword)
    case m: ParseNodes.FinalKeyword => Seq(FinalKeyword)
    case m: ParseNodes.NativeKeyword => Seq(NativeKeyword)

    case t: ParseNodes.ClassType => {
      val children:Seq[AbstractSyntaxNode] = t.children.flatMap(fromParseNode(_))
      Seq(ClassType(fromParseNode(t.children.head).head.asInstanceOf[Name]))
    }
    case t: ParseNodes.InterfaceType => {
      val children:Seq[AbstractSyntaxNode] = t.children.flatMap(fromParseNode(_))
      Seq(ClassOrInterfaceType(fromParseNode(t.children.head).head.asInstanceOf[Name]))
    }

    //  TODO: Implement
    case t: ParseNodes.TrueLiteral => Seq(TrueLiteral)
    case t: ParseNodes.FalseLiteral => Seq(FalseLiteral)

    case t: ParseNodes.ByteKeyword => Seq(ByteKeyword)
    case t: ParseNodes.ShortKeyword => Seq(ShortKeyword)
    case t: ParseNodes.IntKeyword => Seq(IntKeyword)
    case t: ParseNodes.CharKeyword => Seq(CharKeyword)
    case v: ParseNodes.VoidKeyword => Seq(VoidKeyword)

    case s: ParseNodes.SimpleName => Seq(SimpleName(s.children(0).value.get))
    case s: ParseNodes.QualifiedName => 
      Seq(QualifiedName(s.children.flatMap(fromParseNode(_)).flatMap {
        case n: SimpleName => Some(n.value)
        case n: Identifier => Some(n.value)
        case n: QualifiedName => n.value
        case n: AbstractSyntaxNode => throw new SyntaxError("Qualified name contains non-identifiers.")
      }))

    case m: ParseNodes.MethodBody => Seq(MethodBody())  //  TODO: Implement

    //  If the parse node does not map nicely to an ASN, just hand us its children.
    case p: ParseNode => p.children.flatMap(fromParseNode(_))
  }
}
