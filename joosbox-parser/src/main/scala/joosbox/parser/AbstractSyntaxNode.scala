package joosbox.parser

import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

sealed trait AbstractSyntaxNode {
  def parentOption: Option[AbstractSyntaxNode] = None
  def children: List[AbstractSyntaxNode] = List.empty[AbstractSyntaxNode]

  def simpleString(indent: Int = 0): String = {
    (" " * indent) + this.getClass.getSimpleName + "\n" + children.map(_.simpleString(indent + 2)).mkString("")
  }
}

object AbstractSyntaxNode {

  /**
   * AST nodes that can be referenced as a name.
   */
  sealed trait Referenceable extends AbstractSyntaxNode

  sealed trait Literal extends Expression
  case class CharLiteral(value: InputString) extends Literal
  case class StringLiteral(value: InputString) extends Literal
  case object NullLiteral extends Literal
  case object TrueLiteral extends Literal
  case object FalseLiteral extends Literal

  case class Num(value: String, input: InputString) extends Literal {
    def negated: Num = value.headOption match {
      case Some('-') => Num(value.tail, input)
      case _ => Num("-" + value, input)
    }
    def valid: Boolean = {
      try {
        value.toInt
        true
      } catch {
        case e: Exception => false
      }
    }
  }

  case class CompilationUnit(
    packageDeclaration: Option[PackageDeclaration] = None,
    importDeclarations: Seq[ImportDeclaration] = Seq.empty[ImportDeclaration],
    typeDeclaration: Option[TypeDeclaration] = None
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] =
      packageDeclaration.toList ++ importDeclarations.toList ++ typeDeclaration.toList
  }

  case class PackageDeclaration(name: PackageName) extends AbstractSyntaxNode

  sealed trait ImportDeclaration extends AbstractSyntaxNode {
    def name: Name
  }

  case class SingleTypeImportDeclaration(name: Name) extends ImportDeclaration
  case class TypeImportOnDemandDeclaration(name: Name) extends ImportDeclaration

  case class Identifier(value: InputString) extends AbstractSyntaxNode

  case class PackageName(value: InputString, prefix: Option[PackageName] = None) extends Name {
    def niceName = value.value
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: PackageName) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
  }
  case class TypeName(value: InputString, prefix: Option[PackageName] = None) extends Name {
    def niceName = value.value
  }
  case class ExpressionName(value: InputString, prefix: Option[AmbiguousName] = None) extends Name {
    def niceName = value.value
  }
  case class MethodName(value: InputString, prefix: Option[AmbiguousName] = None) extends Name {
    def niceName = value.value
  }

  //  Note we don't need PackageOrTypeName - Java seems to use it for nested classes.
  /*case class PackageOrTypeName(value: InputString, prefix: Option[PackageOrTypeName]) extends Name {
    def niceName = value.value
  }*/
  case class AmbiguousName(value: InputString, prefix: Option[AmbiguousName] = None) extends Name {
    def niceName = value.value
  }

  abstract class ClassBodyDeclaration(
    val name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = modifiers.toList
  }

  //  TODO; implement me properly, not just stubbed out
  case class InterfaceMemberDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,

    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends AbstractSyntaxNode with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList ++ List(memberType)
  }

  case class ClassBody(
    declarations: Seq[ClassBodyDeclaration] = Seq.empty[ClassBodyDeclaration]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = declarations.toList
  }

  case class InterfaceBody(
    declarations: Seq[InterfaceMemberDeclaration] = Seq.empty[InterfaceMemberDeclaration]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = declarations.toList
  }

  sealed trait Primary extends AbstractSyntaxNode
  sealed trait Expression extends Primary
  case class ParenthesizedExpression(expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(expr)
  }

  case class Assignment(
    leftHandSide: AbstractSyntaxNode,
    rightHandSide: Expression
  ) extends StatementExpression {
    override def children: List[AbstractSyntaxNode] = List(leftHandSide, rightHandSide)
  }

  case class ClassCreationStatementExpression(
    creation: ClassCreationPrimary
  ) extends StatementExpression {
    override def children: List[AbstractSyntaxNode] = List(creation)
  }

  case class MethodInvocationExpression(
    invocation: MethodInvocation
  ) extends StatementExpression {
    override def children: List[AbstractSyntaxNode] = List(invocation)
  }

  sealed trait PostfixExpression extends Expression

  sealed trait ConditionalExpression extends Expression
  case class OrExpression(e1: Expression, e2: Expression) extends ConditionalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class AndExpression(e1: Expression, e2: Expression)  extends ConditionalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class BinOrExpression(e1: Expression, e2: Expression)  extends ConditionalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class BinXorExpression(e1: Expression, e2: Expression)  extends ConditionalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class BinAndExpression(e1: Expression, e2: Expression)  extends ConditionalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }

  sealed trait RelationalExpression extends Expression
  case class EqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class NotEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class LessThanExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class LessEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class GreaterThanExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class GreaterEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class InstanceOfExpression(e: Expression, t: ReferenceType)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e) ++ List(t)
  }

  sealed trait ArithmeticExpression extends Expression
  case class AddExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class SubtractExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class MultiplyExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class DivideExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }
  case class ModExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }

  case class NegatedExpression(expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(expr)
  }

  case class FieldAccess(primary: Primary, name: InputString) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(primary)
  }
  case class SimpleArrayAccess(name: Name, expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(name, expr)
  }
  case class ComplexArrayAccess(primary: Primary, expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(primary, expr)
  }

  case object ThisKeyword extends Expression

  case class ArrayCreationPrimary(varType: Type, dimExpr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(varType, dimExpr)
  }
  case class ClassCreationPrimary(classType: ClassType, args: Seq[Expression]) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(classType) ++ args.toList
  }

  sealed trait MethodInvocation extends Expression

  case class SimpleMethodInvocation(
    name: Name,
    args: Seq[Expression] = Seq.empty[Expression]
  ) extends MethodInvocation {
    override def children: List[AbstractSyntaxNode] = List(name) ++ args.toList
  }

  case class ComplexMethodInvocation(
    primary: Primary,
    name: InputString,
    args: Seq[Expression] = Seq.empty[Expression]
  ) extends MethodInvocation {
    override def children: List[AbstractSyntaxNode] = List(primary) ++ args.toList
  }

  sealed trait Type extends AbstractSyntaxNode
  sealed trait Name extends Type with PostfixExpression {
    def niceName: String
  }
  case class SimpleName(value: InputString) extends Name {
    def niceName: String = value.value
  }

  case class QualifiedName(value: Seq[InputString]) extends Name {
    def niceName: String = value.map(_.value).mkString(".")
    def prefixesIncludingSelf: Seq[QualifiedName] = Seq(this) ++ prefixes
    def prefixes: Seq[QualifiedName] = value.size match {
      case 0 => Seq.empty[QualifiedName]
      case 1 => Seq.empty[QualifiedName]
      case _ => {
        val oneSmaller = QualifiedName(value.dropRight(1))
        Seq(oneSmaller) ++ oneSmaller.prefixes
      }
    }

    def toPackageName: PackageName = {
      value.size match {
        case 0 => throw new SyntaxError("PackageName must contain one identifier.")
        case 1 => PackageName(value.last, None)
        case _ => PackageName(value.last, Some(QualifiedName(value.dropRight(1)).toPackageName))
      }
    }

    def toExpressionName: ExpressionName = {
      value.size match {
        case 0 => throw new SyntaxError("ExpressionName must contain one identifier.")
        case 1 => ExpressionName(value.last, None)
        case _ => ExpressionName(value.last, Some(QualifiedName(value.dropRight(1)).toAmbiguousName))
      }
    }

    def toAmbiguousName: AmbiguousName = {
      value.size match {
        case 0 => throw new SyntaxError("AmbiguousName must contain one identifier.")
        case 1 => AmbiguousName(value.last, None)
        case _ => AmbiguousName(value.last, Some(QualifiedName(value.dropRight(1)).toAmbiguousName))
      }
    }
  }

  sealed trait PrimitiveType extends Type
  sealed trait ReferenceType extends Type
  case object VoidKeyword extends Type

  case object BooleanKeyword extends PrimitiveType

  sealed trait NumericType extends PrimitiveType

  case object ByteKeyword extends NumericType
  case object ShortKeyword extends NumericType
  case object IntKeyword extends NumericType
  case object CharKeyword extends NumericType

  case class FormalParameter(
    val name: InputString,
    val varType: Type
  ) extends AbstractSyntaxNode with Referenceable {
    override def children: List[AbstractSyntaxNode] = List(varType)
  }

  case class ArrayType(subtype: Type) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(subtype)
  }
  case class ClassOrInterfaceType(name: Name) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(name)
  }
  case class ClassType(name: Name) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(name)
    def inputString: Seq[InputString] = name match {
        case n: SimpleName => Seq(n.value)
        case q: QualifiedName => q.value
    }
  }
  case class InterfaceType(name: Name) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(name)
    def inputString: Seq[InputString] = name match {
        case n: SimpleName => Seq(n.value)
        case q: QualifiedName => q.value
    }
  }

  sealed trait Modifier extends AbstractSyntaxNode

  sealed trait AccessModifier extends Modifier
  sealed trait NonAccessModifier extends Modifier
  case object StaticKeyword extends NonAccessModifier

  case object PublicKeyword extends AccessModifier
  case object ProtectedKeyword extends AccessModifier

  case object AbstractKeyword extends NonAccessModifier
  case object FinalKeyword extends NonAccessModifier
  case object NativeKeyword extends NonAccessModifier

  sealed abstract class TypeDeclaration(
    val name: InputString,
    val modifiers: Set[Modifier] = Set.empty[Modifier],
    val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType],

    //  This was added so that we can disambiguate identical
    //  classes/interfaces that are declared in different files.
    var parent: Option[CompilationUnit] = None
  ) extends AbstractSyntaxNode with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ interfaces.toList
  }

  case class ClassDeclaration(
    override val name: InputString,
    body: ClassBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    superclass: Option[ClassType] = None,
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      List(body) ++ superclass.toList ++ modifiers.toList ++ interfaces.toList

    override def parentOption: Option[AbstractSyntaxNode] = parent
  }

  case class InterfaceDeclaration(
    override val name: InputString,
    body: InterfaceBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      List(body) ++ modifiers.toList ++ interfaces.toList

    override def parentOption: Option[AbstractSyntaxNode] = parent
  }

  abstract class ClassMemberDeclaration(
    override val name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type
  ) extends ClassBodyDeclaration(name, modifiers) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType)
  }

  case class ConstructorDeclaration(
    override val name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassBodyDeclaration(name, modifiers) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList
  }

  case class MethodDeclaration(
    override val name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ parameters.toList ++ body.toList
  }

  case class FieldDeclaration(
    override val name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    expression: Option[Expression] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ expression.toList
  }

  sealed trait BlockStatement extends AbstractSyntaxNode

  case class LocalVariableDeclaration(
    name: InputString,
    memberType: Type,
    expression: Option[Expression] = None
  ) extends BlockStatement with Referenceable {
    override def children: List[AbstractSyntaxNode] = List(memberType) ++ expression.toList
  }

  case class Block(statements: Seq[BlockStatement] = Seq.empty[BlockStatement]) extends Statement {
    override def children: List[AbstractSyntaxNode] = statements.toList
  }

  case class CastExpression(targetType: Type) extends Expression

  sealed trait Statement extends BlockStatement
  case object EmptyStatement extends Statement

  sealed trait ForInit extends AbstractSyntaxNode
  sealed trait StatementExpression extends Statement with ForInit with Expression

  case class ReturnStatement(expression: Option[Expression] = None) extends Statement {
    override def children: List[AbstractSyntaxNode] = expression.toList
  }

  case class IfStatement(clause: Expression, trueCase: Statement, elseCase: Option[Statement] = None) extends Statement  {
    override def children: List[AbstractSyntaxNode] = List(clause) ++ List(trueCase) ++ elseCase.toList
  }
  case class WhileStatement(clause: Expression, body: Statement) extends Statement {
    override def children: List[AbstractSyntaxNode] = List(clause) ++ List(body)
  }

  case class ForVariableDeclaration(typeDeclaration: Type,
                                    variableName: InputString,
                                    expression: Option[Expression] = None) extends ForInit with Referenceable {
    override def children: List[AbstractSyntaxNode] = List(typeDeclaration) ++ expression.toList
  }
  case class ForStatement(init: Option[ForInit],
                          check: Option[Expression],
                          update: Option[StatementExpression],
                          statement: Statement) extends Statement {
    override def children: List[AbstractSyntaxNode] = init.toList ++ check.toList ++ update.toList ++ List(statement)
  }

  /*
    Parse!
  */

  def parse(node: ParseNode): Seq[AbstractSyntaxNode] = {
    val result = fromParseNode(Nil)(node)
    result.foreach { overflowCheck(_) }
    result
  }

  def overflowCheck(node: AbstractSyntaxNode) {
    node match {
      case num: Num =>
        if (!num.valid) {
          throw new SyntaxError("Overflow/underflow integer: " + num)
        }
      case _ => Unit
    }
    node.children.foreach { x => overflowCheck(x) }
  }

  def fromParseNode(previousNodes: List[ParseNode] = Nil)(node: ParseNode): Seq[AbstractSyntaxNode] = {
  val recursive = fromParseNode(node +: previousNodes) _
  node match {
    case s: ParseNodes.S    => recursive(s.children(1))    //  Grab the compilation unit.
    case p: ParseNodes.BOF  => Seq.empty[AbstractSyntaxNode]
    case p: ParseNodes.EOF  => Seq.empty[AbstractSyntaxNode]

    case c: ParseNodes.CompilationUnit => {
      // Sort our children into ImportDeclarations, TypeDeclarations and PackageDeclaration
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      // The grammar guarantees we should only get one PackageDeclaration,
      // so there's no need to check for multiple here.
      val packageDeclaration: Option[PackageDeclaration] = children.collectFirst { case x: PackageDeclaration => x }
      val importDeclarations: Seq[ImportDeclaration] = children.collect { case x: ImportDeclaration => x }
      val typeDeclarations: Seq[TypeDeclaration] = children.collect { case x: TypeDeclaration => x }

      // Grammar does not guarantee that we only have one type declaration, so make the check here.
      if (typeDeclarations.size > 1) {
        throw new SyntaxError("Cannot define more than one class or interface in a file.");
      }

      val typeDeclaration: Option[TypeDeclaration] = typeDeclarations.headOption

      typeDeclaration match {
        case Some(td: TypeDeclaration) => {
          val cu = CompilationUnit(packageDeclaration, importDeclarations, Some(td))
          td.parent = Some(cu)
          Seq(cu)
        }
        case None => {
          Seq(CompilationUnit(packageDeclaration, importDeclarations, None))
        }
      }
    }

    case p: ParseNodes.PackageDeclaration => {
      val children: Seq[AbstractSyntaxNode] = p.children.flatMap(recursive(_))
      Seq(PackageDeclaration(children.collectFirst { case x: QualifiedName => x.toPackageName }.get))
    }

    case i: ParseNodes.SingleTypeImportDeclaration   => {
      val children: Seq[AbstractSyntaxNode] = i.children.flatMap(recursive(_))
      Seq(SingleTypeImportDeclaration(children.collectFirst { case x: Name => x }.get))
    }

    case i: ParseNodes.TypeImportOnDemandDeclaration => {
      val children: Seq[AbstractSyntaxNode] = i.children.flatMap(recursive(_))
      Seq(TypeImportOnDemandDeclaration(children.collectFirst { case x: Name => x }.get))
    }


    case c: ParseNodes.ClassDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      val name: Identifier = children.collectFirst { case x: Identifier => x }.get

      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      var superclass: Option[ClassType] = children.collectFirst { case x: ClassType => x }
      val interfaces: Seq[InterfaceType] = children.collect { case x: InterfaceType => x }.toSeq
      val body: ClassBody = children.collectFirst { case x: ClassBody => x }.get

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
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val interfaces:Seq[InterfaceType] = children.collect { case x: InterfaceType => x }.toSeq
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

    case c: ParseNodes.ConstructorDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name: QualifiedName = children.collectFirst { case x: QualifiedName => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val parameters: Seq[FormalParameter] = children.collect { case x: FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x: Block => x }

      // TODO: Check if identifier is the same as class name. This doesn't work.
      // val containingClass: ClassDeclaration = previousNodes.collectFirst { case x: ClassDeclaration => x }.get
      // if (!containingClass.name.value.equals(name.value)) {
      //   throw new SyntaxError("Constructor must be the same name as class name: " + containingClass.name.value)
      // }

      // Enforce: No package private methods
      if (!modifiers.contains(PublicKeyword) && !modifiers.contains(ProtectedKeyword)) {
        throw new SyntaxError("Constructor " + name.value + " cannot be package private.")
      }

      // Enforce: A constructor can't be abstract, static, nor final.
      if (modifiers.contains(AbstractKeyword)) {
        throw new SyntaxError("Constructor " + name.value + " cannot be abstract.")
      }

      if (modifiers.contains(StaticKeyword)) {
        throw new SyntaxError("Constructor " + name.value + " cannot be static.")
      }

      if (modifiers.contains(FinalKeyword)) {
        throw new SyntaxError("Constructor " + name.value + " cannot be final.")
      }

      // Enforce: A constructor has to have a body.
      if (body == None) {
        throw new SyntaxError("Constructor " + name.value + " has to have a body.")
      }

      //  Why name.value.head here? We got rid of using SimpleName, but the lexer
      //  guarantees that this QualifiedName will only have one sub-name.
      Seq(ConstructorDeclaration(name.value.head, modifiers, parameters, body))
    }


    case c: ParseNodes.MethodDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x:Type => x }.get
      val parameters: Seq[FormalParameter] = children.collect { case x:FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x:Block => x }

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
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x: Type => x }.get
      val expression: Option[Expression] = children.collectFirst { case x: Expression => x }

      // Enforce: A final field must have an initializer.
      if (modifiers.contains(FinalKeyword)) {
        if (expression.isEmpty) {
          throw new SyntaxError("Field " + name.value + " is final so it must have an initializer.")
        }
      }

      // Enfore: A field can not be package private
      if (!modifiers.contains(PublicKeyword) && !modifiers.contains(ProtectedKeyword)) {
        throw new SyntaxError("Field " + name.value + " cannot be package private.")
      }

      // Enforce: A field can not be abstract
      if (modifiers.contains(AbstractKeyword)) {
        throw new SyntaxError("Field " + name.value + " cannot be abstract.")
      }

      // Enforce: A field can not be native
      if (modifiers.contains(NativeKeyword)) {
        throw new SyntaxError("Field " + name.value + " cannot be native.")
      }

      Seq(FieldDeclaration(name.value, modifiers, memberType, expression))
    }

    case c: ParseNodes.InterfaceMemberDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x: Type => x }.get
      val parameters: Seq[FormalParameter] = children.collect { case x: FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x: Block => x }

      // Enforce: An interface method cannot be static or final.
      if (modifiers.contains(StaticKeyword) || modifiers.contains(FinalKeyword)) {
        throw new SyntaxError("Interface method " + name.value + " cannot be static or final.")
      }

      Seq(InterfaceMemberDeclaration(name.value, modifiers, memberType, parameters, body))
    }

    case i: ParseNodes.Identifier => Seq(Identifier(i.value.get))

    case c: ParseNodes.ClassBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      val classBodyDeclarations: Seq[ClassBodyDeclaration] = children.collect { case x: ClassBodyDeclaration => x }

      val constructor: Seq[ConstructorDeclaration] = classBodyDeclarations.collect { case x: ConstructorDeclaration => x }
      if (constructor.isEmpty) {
        throw new SyntaxError("Constructor is required in class body.");
      }

      Seq(ClassBody(classBodyDeclarations))
    }

    case c: ParseNodes.InterfaceBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      Seq(InterfaceBody(children.collect { case x: InterfaceMemberDeclaration => x }))
    }

    case m: ParseNodes.StaticKeyword => Seq(StaticKeyword)
    case m: ParseNodes.PublicKeyword => Seq(PublicKeyword)
    case m: ParseNodes.ProtectedKeyword => Seq(ProtectedKeyword)
    case m: ParseNodes.AbstractKeyword => Seq(AbstractKeyword)
    case m: ParseNodes.FinalKeyword => Seq(FinalKeyword)
    case m: ParseNodes.NativeKeyword => Seq(NativeKeyword)

    case c: ParseNodes.CastExpression => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val check_children = new ((AbstractSyntaxNode) => AbstractSyntaxNode){
          def apply(x:AbstractSyntaxNode):AbstractSyntaxNode = {
            if(x.children.size > 1) {
              throw new SyntaxError("Casting with invalid cast type.")
            }

            if (x.children.isEmpty) {
              x
            } else {
              x.children.head match {
                case y: Primary => {
                  val expr = y.children.collectFirst { case e: Expression => e }
                  if (!expr.isEmpty) {
                    throw new SyntaxError("Casting with nested expressions is invalid.")
                  }
                  apply(x.children.head)
                }
                case _ => apply(x.children.head)
              }
            }
          }
        }

      children.head match {
        case y: FieldAccess => throw new SyntaxError("Field accesses cannot be cast to.")
        case y: ParenthesizedExpression => throw new SyntaxError("Casts cannot be overly parenthesized.")
        case y: SimpleMethodInvocation => throw new SyntaxError("Method invocations cannot be casted to.")
        case y: ComplexMethodInvocation => throw new SyntaxError("Method invocations cannot be casted to.")
        case _ => ;
      }

      check_children(children.head) match {
        case y: Name => Seq(CastExpression(ClassOrInterfaceType(y)))
        case y: PrimitiveType => Seq(CastExpression(y))
        case _ => throw new SyntaxError("Casting with invalid cast type.")
      }
    }

    case t: ParseNodes.ClassOrInterfaceType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive(_))
      Seq(ClassOrInterfaceType(children.head.asInstanceOf[Name]))
    }
    case t: ParseNodes.ClassType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive(_))
      val child = children.collectFirst { case x: ClassOrInterfaceType => x }.get
      Seq(ClassType(child.name))
    }
    case t: ParseNodes.InterfaceType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive(_))
      val child = children.collectFirst { case x: ClassOrInterfaceType => x }.get
      Seq(InterfaceType(child.name))
    }

    case i: ParseNodes.Num =>
      val input: InputString = i.value.get
      val num = Num(input.value, input)
      Seq(num)

    case u: ParseNodes.UnaryExpression => {
      val children: Seq[AbstractSyntaxNode] = u.children.flatMap(recursive(_))
      u.children match {
        case Seq(minus: ParseNodes.Minus, expr: ParseNodes.UnaryExpression) =>
          val parsed: Seq[AbstractSyntaxNode] = recursive(expr)

          val check_children = new ((ParseNode) => ParseNode){
            def apply(x:ParseNode):ParseNode = {
              if (x.children.size == 1) {
                x match {
                  case y: ParseNodes.Num => y
                  case _ => apply(x.children.head)
                }
              } else {
                x
              }
            }
          }

          val child : Seq[AbstractSyntaxNode] = recursive(check_children(expr))

          if (child.isEmpty) {
            val expression = children.collectFirst { case x: Expression => x }.get
            Seq(NegatedExpression(expression))
          } else {
            child.head match {
              case num: Num => Seq(num.negated)
              case expr: Expression => Seq(NegatedExpression(expr))

              //  If we can't find a Num or Expression, back up and try to parse.
              case _ => parsed match {
                case Seq(expr: Expression) => Seq(NegatedExpression(expr))
                case _ => throw new SyntaxError("Negated unary expression is malformed: " + parsed)
              }
            }
          }
        case _ => children
      }
    }

    case t: ParseNodes.TrueLiteral => Seq(TrueLiteral)
    case t: ParseNodes.FalseLiteral => Seq(FalseLiteral)
    case n: ParseNodes.NullLiteral => Seq(NullLiteral)

    case t: ParseNodes.ByteKeyword => Seq(ByteKeyword)
    case t: ParseNodes.ShortKeyword => Seq(ShortKeyword)
    case t: ParseNodes.IntKeyword => Seq(IntKeyword)
    case t: ParseNodes.CharKeyword => Seq(CharKeyword)
    case v: ParseNodes.VoidKeyword => Seq(VoidKeyword)
    case b: ParseNodes.BooleanKeyword => Seq(BooleanKeyword)
    case a: ParseNodes.ArrayType => {
      val children:Seq[AbstractSyntaxNode] = a.children.flatMap(recursive(_))
      children.headOption match {
        case Some(x: Type) => Seq(ArrayType(x))
        case _ => throw new SyntaxError("Array type contains non-type node.")
      }
    }
    case p: ParseNodes.FormalParameter => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive(_))

      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val varType: Type = children.collectFirst { case x: Type => x }.get

      Seq(FormalParameter(name.value, varType))
    }

    case c: ParseNodes.CharLiteral => Seq(CharLiteral(c.value.get))
    case s: ParseNodes.StringLiteral => Seq(StringLiteral(s.value.get))

    case s: ParseNodes.SimpleName => Seq(QualifiedName(Seq(s.children(0).value.get)))
    case s: ParseNodes.QualifiedName =>
      Seq(QualifiedName(s.children.flatMap(recursive(_)).flatMap {
        case n: SimpleName => Some(n.value)
        case n: Identifier => Some(n.value)
        case n: QualifiedName => n.value
        case _ => throw new SyntaxError("Qualified name contains non-identifiers.")
      }))

    case b: ParseNodes.Block => {
      val children: Seq[AbstractSyntaxNode] = b.children.flatMap(recursive(_))
      val blockStatements: Seq[BlockStatement] = children.collect { case x: BlockStatement => x }
      Seq(Block(blockStatements))
    }

    case b: ParseNodes.ConstructorBody => {
      val children: Seq[AbstractSyntaxNode] = b.children.flatMap(recursive(_))
      val blockStatements: Seq[BlockStatement] = children.collect { case x: BlockStatement => x }
      Seq(Block(blockStatements))
    }

    case ParseNodes.VariableDeclaratorId(List(id: ParseNodes.Identifier,
                                              lb: ParseNodes.LeftBracket,
                                              rb: ParseNodes.RightBracket), _) => {
      // Enforce: Array brackets ('[]') are not allowed to occur in the name of a variable being declared.
      throw new SyntaxError("Variable declaration cannot contain array syntax: " + id.value)
    }

    case l: ParseNodes.LocalVariableDeclaration => {
      val children:Seq[AbstractSyntaxNode] = l.children.flatMap(recursive(_))

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val memberType:Type = children.collectFirst { case x: Type => x }.get
      val expression:Option[Expression] = children.collectFirst { case x:Expression => x }

      Seq(LocalVariableDeclaration(name.value, memberType, expression))
    }

    case i: ParseNodes.IfThenStatement => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive(_))
      Seq(IfStatement(children(0).asInstanceOf[Expression], children(1).asInstanceOf[Statement]))
    }

    case i: ParseNodes.IfThenElseStatement => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive(_))
      val clauses:Seq[Statement] = children.collect { case x: Statement => x }
      val ifClause:Statement = clauses.head
      val elseClause:Option[Statement] = clauses.drop(1).headOption
      Seq(IfStatement(children(0).asInstanceOf[Expression], ifClause, elseClause))
    }

    case i: ParseNodes.IfThenElseStatementNoShortIf => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive(_))
      val clauses:Seq[Statement] = children.collect { case x: Statement => x }
      val ifClause:Statement = clauses.head
      val elseClause:Option[Statement] = clauses.drop(1).headOption
      Seq(IfStatement(children(0).asInstanceOf[Expression], ifClause, elseClause))
    }

    case w: ParseNodes.WhileStatement => {
      val children:Seq[AbstractSyntaxNode] = w.children.flatMap(recursive(_))
      Seq(WhileStatement(children(0).asInstanceOf[Expression], children(1).asInstanceOf[Statement]))
    }

    case f: ParseNodes.ForStatement => {
      val children:Seq[AbstractSyntaxNode] = f.children.flatMap(recursive(_))

      val init: Option[ForInit] = children.collectFirst { case x: ForInit => x }
      val check: Option[Expression] = children.collectFirst { case x: Expression => x }

      var statements: Seq[Statement] = children.collect { case x: Statement => x }
      val update: Option[StatementExpression] = statements.collectFirst { case x: StatementExpression => x }
      if (!update.isEmpty) {
        statements = statements diff List(update.get)
      }
      val statement: Statement = statements.collectFirst { case x: Statement => x }.get
      Seq(ForStatement(init, check, update, statement))
    }

    case f: ParseNodes.ForStatementNoShortIf => {
      val children:Seq[AbstractSyntaxNode] = f.children.flatMap(recursive(_))
      val init: Option[ForInit] = children.collectFirst { case x: ForInit => x }
      val check: Option[Expression] = children.collectFirst { case x: Expression => x }
      val update: Option[StatementExpression] = children.collectFirst { case x: StatementExpression => x }
      val statement: Statement = children.collectFirst { case x: Statement => x }.get
      Seq(ForStatement(init, check, update, statement))
    }

    case f: ParseNodes.ForInit => {
      f.children match {
        case Seq(t: ParseNodes.Type, v: ParseNodes.VariableDeclarator) => {
          val vChildren = v.children.flatMap(recursive(_))

          val memberType: Type = t.children.flatMap(recursive(_)).collectFirst { case x: Type => x }.get
          val name: Identifier = vChildren.collectFirst { case x: Identifier => x }.get
          val expression: Option[Expression] = vChildren.collectFirst { case x: Expression => x }

          Seq(ForVariableDeclaration(memberType, name.value, expression))
        }
        case _ => f.children.flatMap(recursive(_))
      }
    }

    case r: ParseNodes.ReturnStatement => {
      val children:Seq[AbstractSyntaxNode] = r.children.flatMap(recursive(_))
      val expression: Option[Expression] = children.collectFirst { case x: Expression => x }
      Seq(ReturnStatement(expression))
    }


    case e: ParseNodes.ConditionalOrExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.ConditionalOrExpression, s: ParseNodes.LogicalOr, e2: ParseNodes.ConditionalAndExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(OrExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid or expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.ConditionalAndExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.ConditionalAndExpression, s: ParseNodes.LogicalAnd, e2: ParseNodes.InclusiveOrExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(AndExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid and expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.InclusiveOrExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.InclusiveOrExpression, s: ParseNodes.BinaryOr, e2: ParseNodes.ExclusiveOrExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(BinOrExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid inclusive-or expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.ExclusiveOrExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.ExclusiveOrExpression, s: ParseNodes.BinaryXor, e2: ParseNodes.AndExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(BinXorExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid exclusive-or expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.AndExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.AndExpression, s: ParseNodes.BinaryAnd, e2: ParseNodes.EqualityExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(BinAndExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid binary-and expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.EqualityExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.EqualityExpression, s: ParseNodes.Equal, e2: ParseNodes.RelationalExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(EqualExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid equal expression")
          }
        }
         case Seq(e1: ParseNodes.EqualityExpression, s: ParseNodes.NotEqual, e2: ParseNodes.RelationalExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(NotEqualExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid not-equal expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.RelationalExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.RelationalExpression, s: ParseNodes.LessThan, e2: ParseNodes.AdditiveExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(LessThanExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid relational expression " + children)
          }
        }
        case Seq(e1: ParseNodes.RelationalExpression, s: ParseNodes.LessEqual, e2: ParseNodes.AdditiveExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(LessEqualExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid relational expression")
          }
        }
        case Seq(e1: ParseNodes.RelationalExpression, s: ParseNodes.GreaterThan, e2: ParseNodes.AdditiveExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(GreaterThanExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid relational expression")
          }
        }
        case Seq(e1: ParseNodes.RelationalExpression, s: ParseNodes.GreaterEqual, e2: ParseNodes.AdditiveExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(GreaterEqualExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid relational expression")
          }
        }
        case Seq(e1: ParseNodes.RelationalExpression, s: ParseNodes.InstanceofKeyword, r1: ParseNodes.ReferenceType) => {
          children match {
            case Seq(expr: Expression, ref: ReferenceType) => Seq(InstanceOfExpression(expr, ref))
            case _ => throw new SyntaxError("Invalid relational expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.AdditiveExpression => {
      val children = e.children.flatMap(recursive(_))

      e.children match {
        case Seq(e1: ParseNodes.AdditiveExpression, s: ParseNodes.Plus, e2: ParseNodes.MultiplicativeExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(AddExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid add expression") 
          }
        }
        case Seq(e1: ParseNodes.AdditiveExpression, s: ParseNodes.Minus, e2: ParseNodes.MultiplicativeExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(SubtractExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid subtract expression")
          }
        }
        case _ => children
      }
    }

    case e: ParseNodes.MultiplicativeExpression => {
      val children = e.children.flatMap(recursive(_))
      e.children match {
        case Seq(e1: ParseNodes.MultiplicativeExpression, s: ParseNodes.Star, e2: ParseNodes.UnaryExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(MultiplyExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid multiply expression")
          }
        }
        case Seq(e1: ParseNodes.MultiplicativeExpression, s: ParseNodes.Divide, e2: ParseNodes.UnaryExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(DivideExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid divide expression")
          }
        }
        case Seq(e1: ParseNodes.MultiplicativeExpression, s: ParseNodes.Modulo, e2: ParseNodes.UnaryExpression) => {
          children match {
            case Seq(expr1: Expression, expr2: Expression) => Seq(ModExpression(expr1, expr2))
            case _ => throw new SyntaxError("Invalid modulo expression")
          }
        }
        case _ => children
      }
    }

    //case e: ParseNodes.AssignmentExpression => Seq(AssignmentExpression(e.children.flatMap(recursive(_))))

    case e: ParseNodes.Assignment => {
      e.children match {
        case Seq(l: ParseNodes.LeftHandSide, _, r: ParseNodes.AssignmentExpression) => {
          val leftChildren = l.children.flatMap(recursive(_))
          val rightChildren = r.children.flatMap(recursive(_))

          val right: Expression = rightChildren.collectFirst { case x: Expression => x }.get
          leftChildren.headOption match {
            case Some(left: QualifiedName) => Seq(Assignment(left.toExpressionName, right))
            case Some(left: FieldAccess) => Seq(Assignment(left, right))
            case Some(left: SimpleArrayAccess) => Seq(Assignment(left, right))
            case Some(left: ComplexArrayAccess) => Seq(Assignment(left, right))
            case _ => {
              throw new SyntaxError("Assignment has invalid left hand side.")
            }
          }

        }
        case _ => e.children.flatMap(recursive(_))
      }
    }

    case a: ParseNodes.FieldAccess => {
      val children:Seq[AbstractSyntaxNode] = a.children.flatMap(recursive(_))

      val primary:Primary = children.collectFirst { case x: Primary => x }.get
      val name:Identifier = children.collectFirst { case x: Identifier => x }.get

      Seq(FieldAccess(primary, name.value))
    }
    case a: ParseNodes.ArrayAccess => {
      val children = a.children.flatMap(recursive(_))
      a.children match {
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftBracket, e: ParseNodes.Expression, r: ParseNodes.RightBracket) => {
          val name: Name = children.collectFirst { case x: Name => x }.get
          val expr: Expression = children.collectFirst { case x: Expression => x }.get
          Seq(SimpleArrayAccess(name, expr))
        }
        case Seq(p: ParseNodes.PrimaryNoNewArray, l: ParseNodes.LeftBracket, e: ParseNodes.Expression, r: ParseNodes.RightBracket) => {
          val primary: Primary = children.collectFirst { case x: Primary => x }.get
          val expr: Expression = children.collectFirst { case x: Expression => x }.get
          Seq(ComplexArrayAccess(primary, expr))
        }
        case _ => a.children.flatMap(recursive(_))
      }
    }

    case ParseNodes.PrimaryNoNewArray(List(lp: ParseNodes.LeftParen, content: ParseNode, rp: ParseNodes.RightParen), _) => 
      Seq(ParenthesizedExpression(content.children.flatMap(recursive(_)).collectFirst {case x: Expression => x}.get))

    case t: ParseNodes.ThisKeyword => Seq(ThisKeyword)

    case p: ParseNodes.StatementExpression => {
      val children: Seq[AbstractSyntaxNode] = p.children.flatMap(recursive(_))
      val expression: Expression = children.collectFirst { case x: Expression => x }.get

      expression match {
        case node: ClassCreationPrimary => Seq(ClassCreationStatementExpression(node))
        case node: MethodInvocation => Seq(MethodInvocationExpression(node))
        case _ => children
      }
    }

    case p: ParseNodes.ArrayCreationExpression => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive(_))

      val varType:Type = children.collectFirst { case x: Type => x }.get
      val dimExpr:Expression = children.collectFirst { case x: Expression => x }.get

      Seq(ArrayCreationPrimary(varType, dimExpr))
    }
    case p: ParseNodes.ClassInstanceCreationExpression => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive(_))

      val classType:ClassType = children.collectFirst { case x: ClassType => x }.get
      val args:Seq[Expression] = children.collect { case x: Expression => x }

      Seq(ClassCreationPrimary(classType, args))
    }

    case m: ParseNodes.MethodInvocation => {
      val children = m.children.flatMap(recursive(_))
      m.children match {
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftParen, a: ParseNodes.ArgumentList, r: ParseNodes.RightParen) => {
          val name: Name = children.collectFirst { case x: Name => x }.get
          val args: Seq[Expression] = children.drop(1).collect { case x: Expression => x }
          Seq(SimpleMethodInvocation(name, args))
        }
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftParen, r: ParseNodes.RightParen) => {
          val name: Name = children.collectFirst { case x: Name => x }.get
          Seq(SimpleMethodInvocation(name))
        }

        case Seq(p: ParseNodes.Primary, d: ParseNodes.Dot, i: ParseNodes.Identifier, l: ParseNodes.LeftParen, e: ParseNodes.ArgumentList, r: ParseNodes.RightParen) => {
          val primary: Primary = children.collectFirst { case x: Primary => x }.get
          val args: Seq[Expression] = children.collect { case x: Expression => x }
          val name: Identifier = children.collectFirst { case x: Identifier => x }.get
          Seq(ComplexMethodInvocation(primary, name.value, args))
        }
        case Seq(p: ParseNodes.Primary, d: ParseNodes.Dot, i: ParseNodes.Identifier, l: ParseNodes.LeftParen, r: ParseNodes.RightParen) => {
          val primary: Primary = children.collectFirst { case x: Primary => x }.get
          val name: Identifier = children.collectFirst { case x: Identifier => x }.get
          Seq(ComplexMethodInvocation(primary, name.value))
        }
        case _ => m.children.flatMap(recursive(_))
      }
    }

    // If the parse node does not map nicely to an ASN, just hand us its children.
    case p: ParseNode => p.children.flatMap(recursive(_))
  }
  }
}
