package joosbox.parser

import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError
import scala.runtime.ScalaRunTime

sealed trait AbstractSyntaxNode {
  def parentOption: Option[AbstractSyntaxNode] = None
  def children: List[AbstractSyntaxNode] = List.empty[AbstractSyntaxNode]

  def simpleString(indent: Int = 0): String = {
    (" " * indent) + this.getClass.getSimpleName + "\n" + children.map(_.simpleString(indent + 2)).mkString("")
  }
  def symbolName: String = (List(this.getClass.getSimpleName) ++ children.map(_.symbolName)).mkString("_")

  var scope: Option[Environment] = None
  var stringify: Boolean = false
  var constantValue : Option[AbstractSyntaxNode.ConstantValue] = None

  def astHashCode: Int = scope.hashCode + constantValue.hashCode
  override def hashCode: Int = super.hashCode
}

object AbstractSyntaxNode {

  sealed trait ConstantValue extends AbstractSyntaxNode {
    def value : String
    def toConstantInt : Int = throw new SyntaxError("Unimplemented toConstantInt")
    def toConstantString : String = value
  }
  case class ConstantString(value: String) extends ConstantValue
  case class ConstantChar(value: String) extends ConstantValue {
    override def toConstantInt : Int = {
      value match {
        case "'\n'" => '\n'.toInt
        case "'\r'" => '\r'.toInt
        case "'\t'" => '\t'.toInt
        case "'\b'" => '\b'.toInt
        case "'\f'" => '\f'.toInt
        case "'\"'" => '\"'.toInt
        case "'\\'" => '\\'.toInt
        case "'\''" => '\''.toInt
        case s =>
          var sub = s.substring(1, s.length - 1)
          if (sub.length == 1) {
            sub.toList.head.toInt
          } else {
            // octal escape sequence
            sub = s.substring(1, s.length)
            Integer.parseInt(sub, 8)
          }
      }
    }
    override def toConstantString : String = {
      value.substring(1, value.length - 1)
    }
  }
  case class ConstantBool(value: String) extends ConstantValue
  case class ConstantNum(value: String) extends ConstantValue {
    override def toConstantInt : Int = {
      value.toInt
    }
  }
  case class ConstantNull(value: String) extends ConstantValue

  /**
   * AST nodes that can be referenced as a name.
   */
  sealed trait Referenceable extends AbstractSyntaxNode

  sealed trait Literal extends Expression
  case class CharLiteral(value: InputString) extends Literal {
    override def symbolName: String = "char"
  }
  case class StringLiteral(value: InputString) extends Literal {
    override def symbolName: String = "string"
    var index : Option[Integer] = None
    def stringVal:String = {
      value.value.substring(1, value.value.length - 1)
    }
  }
  case class NullLiteral() extends Literal {
    override def symbolName: String = "null"
  }
  case class TrueLiteral() extends Literal {
    override def symbolName: String = "true"
  }
  case class FalseLiteral() extends Literal {
    override def symbolName: String = "false"
  }

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
    typeDeclaration: TypeDeclaration
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] =
      packageDeclaration.toList ++ importDeclarations.toList ++ List(typeDeclaration)
    def assemblyFileName: String =
      typeDeclaration.fullyQualifiedName.get.toQualifiedName.value.map(_.value).mkString("")
  }

  object PackageDeclaration {
    val implicitPackage = PackageDeclaration(PackageName(InputString("")))
  }
  case class PackageDeclaration(name: PackageName) extends AbstractSyntaxNode

  sealed trait ImportDeclaration extends AbstractSyntaxNode {
    def name: Name
  }

  case class SingleTypeImportDeclaration(name: TypeName) extends ImportDeclaration
  case class TypeImportOnDemandDeclaration(name: PackageName) extends ImportDeclaration

  case class Identifier(value: InputString) extends AbstractSyntaxNode

  case class PackageName(value: InputString, prefix: Option[PackageName] = None) extends Name {
    override def hashCode: Int = value.hashCode + prefix.hashCode
    def niceName = value.value
    def isAmbiguous: Boolean = false
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: Name) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
    override def children: List[AbstractSyntaxNode] = prefix match {
      case Some(n: Name) => List(n) ++ n.children
      case None => List.empty[AbstractSyntaxNode]
    }
  }
  case class TypeName(value: InputString, prefix: Option[PackageName] = None) extends Name {
    override def hashCode: Int = value.hashCode + prefix.hashCode
    def niceName = value.value
    def isAmbiguous: Boolean = false
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: Name) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
    override def children: List[AbstractSyntaxNode] = prefix match {
      case Some(n: Name) => List(n) ++ n.children
      case None => List.empty[AbstractSyntaxNode]
    }
  }
  case class ExpressionName(value: InputString, prefix: Option[Name] = None) extends Name {
    override def hashCode: Int = value.hashCode + prefix.hashCode
    def niceName = value.value
    override def symbolName = value.value
    def isAmbiguous: Boolean = prefix match {
      case Some(n: Name) => n.isAmbiguous
      case None => false
    }
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: Name) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
    override def children: List[AbstractSyntaxNode] = prefix match {
      case Some(n: Name) => List(n) ++ n.children
      case None => List.empty[AbstractSyntaxNode]
    }
  }
  case class MethodName(value: InputString, prefix: Option[Name] = None) extends Name {
    override def hashCode: Int = value.hashCode + prefix.hashCode
    def niceName = value.value
    def isAmbiguous: Boolean = prefix match {
      case Some(n: Name) => n.isAmbiguous
      case None => false
    }
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: Name) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
    override def children: List[AbstractSyntaxNode] = prefix match {
      case Some(n: Name) => List(n) ++ n.children
      case None => List.empty[AbstractSyntaxNode]
    }
  }

  case class AmbiguousName(value: InputString, prefix: Option[Name] = None) extends Name {
    override def hashCode: Int = value.hashCode + prefix.hashCode
    def niceName = value.value
    def isAmbiguous: Boolean = true
    def toSeq: Seq[InputString] = prefix match {
      case Some(p: Name) => p.toSeq ++ Seq(value)
      case None => Seq(value)
    }
    def toQualifiedName: QualifiedName = QualifiedName(toSeq)
    override def children: List[AbstractSyntaxNode] = prefix match {
      case Some(n: Name) => List(n) ++ n.children
      case None => List.empty[AbstractSyntaxNode]
    }
  }

  abstract class ClassBodyDeclaration(
    val name: Name,
    modifiers: Set[Modifier] = Set.empty[Modifier]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = List(name) ++ modifiers.toList
  }

  case class InterfaceMemberDeclaration(
    name: MethodName,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends AbstractSyntaxNode with TypeMethodDeclaration with Referenceable with MethodOrConstructorDeclaration{
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList ++ List(memberType)

    override def niceName: String = name.niceName
    override def symbolName: String = {
      (
        scope.get.getEnclosingCompilationUnit.get
          .asInstanceOf[CompilationUnit]
          .typeDeclaration.asInstanceOf[InterfaceDeclaration]
          .name.niceName + "__"
        + name.niceName
        + parameters.map(_.varType.symbolName).mkString("_")
      ).replaceAll("""^\s+(?m)""", "")
    }
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

  sealed trait ConditionalExpression extends Expression {
    def e1 : Expression
    def e2 : Expression
  }
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

  sealed trait RelationalExpression extends Expression {
    override def symbolName: String = ("RelationalExpression_0x" + hashCode.toHexString)
  }
  case class EqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class NotEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class LessThanExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class LessEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class GreaterThanExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class GreaterEqualExpression(e1: Expression, e2: Expression)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }
  case class InstanceOfExpression(e: Expression, t: ReferenceType)  extends RelationalExpression {
    override def children: List[AbstractSyntaxNode] = List(e) ++ List(t)
    override def hashCode: Int = scope.hashCode + constantValue.hashCode + super.hashCode
  }

  sealed trait ArithmeticExpression extends Expression {
    def e1 : Expression
    def e2 : Expression
  }
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
    override def symbolName: String = ("DivideExpression_0x" + hashCode.toHexString)
  }
  case class ModExpression(e1: Expression, e2: Expression) extends ArithmeticExpression {
    override def children: List[AbstractSyntaxNode] = List(e1) ++ List(e2)
  }

  case class NegatedExpression(expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(expr)
  }
  case class LogicalNotExpression(expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(expr)
  }

  case class FieldAccess(primary: Primary, name: InputString) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(primary)
  }
  case class SimpleArrayAccess(name: ExpressionName, expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(name, expr)
  }
  case class ComplexArrayAccess(primary: Primary, expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(primary, expr)
  }

  case class ThisKeyword() extends Expression

  case class ArrayCreationPrimary(varType: Type, dimExpr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(varType, dimExpr)
  }
  case class ClassCreationPrimary(classType: ClassType, args: Seq[Expression]) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(classType) ++ args.toList
  }

  sealed trait MethodInvocation extends Expression

  case class SimpleMethodInvocation(
    name: MethodName,
    args: Seq[Expression] = Seq.empty[Expression]
  ) extends MethodInvocation {
    override def children: List[AbstractSyntaxNode] = List(name) ++ args.toList
  }

  case class ComplexMethodInvocation(
    primary: Primary,
    name: MethodName,
    args: Seq[Expression] = Seq.empty[Expression]
  ) extends MethodInvocation {
    override def children: List[AbstractSyntaxNode] = List(primary) ++ args.toList
  }

  sealed trait Type extends AbstractSyntaxNode {
    var writeable: Boolean = true
  }

  sealed trait Name extends PostfixExpression {
    def niceName: String
    def isAmbiguous: Boolean
    def toSeq: Seq[InputString]
  }

  case object CommonNames {
    lazy val JavaLangObject = QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("Object")))
    lazy val JavaLangString = QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("String")))
    lazy val JavaLangCloneable = QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("Cloneable")))
    lazy val JavaIOSerializable = QualifiedName(Seq(InputString("java"), InputString("io"), InputString("Serializable")))

    //  one of: java.lang.Object, java.lang.Cloneable, java.io.Serializable
    def doesAcceptArrayAssignment(r: ReferenceType): Boolean = {
      (r match {
        case c: ClassType => Some(c.fullyQualified.asInstanceOf[ClassType].name.toQualifiedName)
        case ci: ClassOrInterfaceType => ci.fullyQualified match {
          case c: ClassType => Some(c.fullyQualified.asInstanceOf[ClassType].name.toQualifiedName)
          case i: InterfaceType => Some(i.fullyQualified.asInstanceOf[InterfaceType].name.toQualifiedName)
          case _ => throw new SyntaxError("Array assignment check resulted in non-class or interface type.")
        }
        case i: InterfaceType => Some(i.fullyQualified.asInstanceOf[InterfaceType].name.toQualifiedName)
        case _ => None
      }) match {
        case Some(JavaLangObject)
           | Some(JavaLangCloneable)
           | Some(JavaIOSerializable) => true
        case _ => false
      }
    }


  }

  //  This no longer inherits from Name, as it's really just a helper atm.
  case class QualifiedName(value: Seq[InputString]) extends AbstractSyntaxNode {
    override def hashCode: Int = value.hashCode

    def niceName: String = value.map(_.value).mkString(".")
    def isAmbiguous: Boolean = false
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

    def toMethodName: MethodName = {
      value.size match {
        case 0 => throw new SyntaxError("MethodName must contain one identifier.")
        case 1 => MethodName(value.last, None)
        case _ =>
          MethodName(value.last, Some(QualifiedName(value.dropRight(1)).toAmbiguousName))
      }
    }
    def toAmbiguousName: AmbiguousName = {
      value.size match {
        case 0 => throw new SyntaxError("AmbiguousName must contain one identifier.")
        case 1 => AmbiguousName(value.last, None)
        case _ => AmbiguousName(value.last, Some(QualifiedName(value.dropRight(1)).toAmbiguousName))
      }
    }
    def toTypeName: TypeName = {
      value.size match {
        case 0 => throw new SyntaxError("TypeName must contain one identifier.")
        case 1 => TypeName(value.last, None)
        case _ => TypeName(value.last, Some(QualifiedName(value.dropRight(1)).toPackageName))
      }
    }
  }

  sealed trait PrimitiveType extends Type
  sealed trait ReferenceType extends Type {
    def fullyQualified: ReferenceType
  }
  sealed trait ReferenceNonArrayType extends ReferenceType {
    def name: TypeName
    def fullyQualifiedName: QualifiedName = fullyQualified.asInstanceOf[ReferenceNonArrayType].name.toQualifiedName
    def node: Option[TypeDeclaration] =
      scope match {
        case Some(env: ScopeEnvironment)
          => env.lookup(EnvironmentLookup.lookupFromName(name)).asInstanceOf[Option[TypeDeclaration]]
        case _
          => throw new SyntaxError("Could not fetch node for type " + this)
      }
  }

  case class VoidKeyword() extends Type {
    override def symbolName: String = "void"
  }

  case class BooleanKeyword() extends PrimitiveType {
    override def symbolName: String = "bool"
  }

  sealed trait NumericType extends PrimitiveType

  case class ByteKeyword() extends NumericType {
    override def symbolName: String = "byte"
  }
  case class ShortKeyword() extends NumericType {
    override def symbolName: String = "short"
  }
  case class IntKeyword() extends NumericType {
    override def symbolName: String = "int"
  }
  case class CharKeyword() extends NumericType {
    override def symbolName: String = "char"
  }

  case class FormalParameter(
    val name: ExpressionName,
    val varType: Type
  ) extends AbstractSyntaxNode with Referenceable {
    override def children: List[AbstractSyntaxNode] = List(varType)
    override def toString: String = "FormalParameter(" + name + ", " + varType + ")"
  }

  case class ArrayType(subtype: Type) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(subtype)
    def fullyQualified: ArrayType = (scope orElse subtype.scope) match {
      case Some(s: ScopeEnvironment) => s.fullyQualifyType(this).asInstanceOf[ArrayType]
      case _ =>
        throw new SyntaxError("Failed to fully qualify type: " + this)
    }
  }
  case class ClassOrInterfaceType(name: TypeName) extends ReferenceNonArrayType {
    override def children: List[AbstractSyntaxNode] = List(name)
    def fullyQualified: ReferenceType = (scope orElse name.scope) match {
      case Some(s: ScopeEnvironment) => s.fullyQualifyType(this).asInstanceOf[ReferenceType]
      case _ =>
        throw new SyntaxError("Failed to fully qualify type: " + this)
    }
    override def symbolName: String = name.niceName
  }
  case class ClassType(name: TypeName) extends ReferenceNonArrayType {
    override def children: List[AbstractSyntaxNode] = List(name)
    def inputString: Seq[InputString] = name.toSeq
    def fullyQualified: ClassType = (scope orElse name.scope) match {
      case Some(s: ScopeEnvironment) => s.fullyQualifyType(this).asInstanceOf[ClassType]
      case _ =>
        throw new SyntaxError("Failed to fully qualify type: " + this)
    }
    override def symbolName: String = name.niceName
  }
  case class InterfaceType(name: TypeName) extends ReferenceNonArrayType {
    override def children: List[AbstractSyntaxNode] = List(name)
    def inputString: Seq[InputString] = name.toSeq
    def fullyQualified: InterfaceType = (scope orElse name.scope) match {
      case Some(s: ScopeEnvironment) => s.fullyQualifyType(this).asInstanceOf[InterfaceType]
      case _ =>
        throw new SyntaxError("Failed to fully qualify type: " + this)
    }
    override def symbolName: String = name.niceName
  }

  sealed trait Modifier extends AbstractSyntaxNode

  sealed trait AccessModifier extends Modifier
  sealed trait NonAccessModifier extends Modifier
  case class StaticKeyword() extends NonAccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "static"
  }

  case class PublicKeyword() extends AccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "public"
  }
  case class ProtectedKeyword() extends AccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "protected"
  }

  case class AbstractKeyword() extends NonAccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "abstract"
  }
  case class FinalKeyword() extends NonAccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "final"
  }
  case class NativeKeyword() extends NonAccessModifier {
    //  All instances of modifiers should be hashCode equivalent.
    override def hashCode: Int = this.getClass.hashCode()
    override def symbolName: String = "native"
  }

  sealed trait MethodOrConstructorDeclaration extends AbstractSyntaxNode

  sealed abstract class TypeDeclaration(
    val name: TypeName,
    val modifiers: Set[Modifier] = Set.empty[Modifier],
    val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType],

    //  This was added so that we can disambiguate identical
    //  classes/interfaces that are declared in different files.
    var parent: Option[CompilationUnit] = None,
    var fullyQualifiedName: Option[TypeName] = None
  ) extends AbstractSyntaxNode with Referenceable {
    def runtimeTag: Int = hashCode
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ interfaces.toList
  }

  case class ClassDeclaration(
    override val name: TypeName,
    body: ClassBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    superclass: Option[ClassType] = None,
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      List(body) ++ superclass.toList ++ modifiers.toList ++ interfaces.toList

    override def parentOption: Option[AbstractSyntaxNode] = parent
    override def symbolName: String = "class_" + name.niceName + "_0x" + hashCode.toHexString

    def isSameOrSubclassOf(decl: ClassDeclaration): Boolean = decl match {
      // The class declarations are identical.
      case d if d == this => true
      case _ => superclass match {
        // No superclass, can't be a superclass.
        case None => false
        case Some(classType: ClassType) => {
          // Search scope for the superclass by name to get class declaration reference.
          decl.scope.get.lookup(EnvironmentLookup.lookupFromName(classType.name)) match {
            case Some(superDecl: ClassDeclaration) => superDecl.isSameOrSubclassOf(decl)
            case _ => throw new SyntaxError("Could not resolve superclass in scope.")
          }
        }
      }
    }

    def staticFieldsForVtable: Seq[FieldDeclaration] = {
      body.declarations.collect { case field: FieldDeclaration if field.isStatic => field }
    }

    //  A list of method declarations, in order, for the generated vtable.
    def methodsForVtable: Seq[MethodOrConstructorDeclaration] = {
      val superclassMethods: Seq[MethodOrConstructorDeclaration] = superclass match {
        case Some(c: ClassType) => scope match {
          case Some(scope: ScopeEnvironment) => {
            scope.lookup(EnvironmentLookup.lookupFromName(c.name)) match {
              case Some(scd: ClassDeclaration) => scd.methodsForVtable
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          }
          case _ => throw new SyntaxError("Could not find scope environment to get vtable methods.")
        }
        case None => Seq.empty[MethodOrConstructorDeclaration]
      }

      val interfaceMethods: Seq[MethodOrConstructorDeclaration] = scope match {
        case Some(scope: ScopeEnvironment) => {
          interfaces.flatMap(m => {
            scope.lookup(EnvironmentLookup.lookupFromName(m.name)) match {
              case Some(id: InterfaceDeclaration) => id.methodsForVtable
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          })
        }
        case _ => throw new SyntaxError("Could not find scope environment to get vtable methods.")
      }

      (
        body.declarations.collect {
          case c: ConstructorDeclaration => c
          case m: MethodDeclaration if !m.isStatic =>  m
        }
        ++ superclassMethods
        ++ interfaceMethods
      )
    }

    def instanceOfList: Seq[String] = {
      val superinstances = superclass match {
        case Some(ct: ClassType) => scope match {
          case Some(scope: ScopeEnvironment) => {
            scope.lookup(EnvironmentLookup.lookupFromName(ct.name)) match {
              case Some(scd: ClassDeclaration) => scd.instanceOfList
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          }
          case _ => Seq.empty[String]
        }
        case None => Seq.empty[String]
      }

      val interfacenames: Seq[String] = scope match {
        case Some(scope: ScopeEnvironment) => {
          interfaces.flatMap(m => {
            scope.lookup(EnvironmentLookup.lookupFromName(m.name)) match {
              case Some(id: InterfaceDeclaration) => id.instanceOfList
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          })
        }
        case _ => throw new SyntaxError("Could not find scope environment to get vtable methods.")
      }

      Seq[String](symbolName) ++ superinstances ++ interfacenames
    }
  }

  case class InterfaceDeclaration(
    override val name: TypeName,
    body: InterfaceBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val interfaces: Seq[InterfaceType] = Seq.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      List(body) ++ modifiers.toList ++ interfaces.toList

    override def symbolName: String = "interface_" + name.niceName + "_0x" + hashCode.toHexString

    override def parentOption: Option[AbstractSyntaxNode] = parent

    def methodsForVtable: Seq[MethodOrConstructorDeclaration] = {
      val interfaceMethods = scope match {
        case Some(scope: ScopeEnvironment) => {
          interfaces.flatMap(m => {
            scope.lookup(EnvironmentLookup.lookupFromName(m.name)) match {
              case Some(id: InterfaceDeclaration) => id.methodsForVtable
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          })
        }
        case _ => throw new SyntaxError("Could not find scope environment to get vtable methods.")
      }
      (
        body.declarations.collect {
          case im: InterfaceMemberDeclaration if !im.isStatic => im
        }
        ++ interfaceMethods
      )
    }

    def instanceOfList: Seq[String] = {
      val interfaceNames = scope match {
        case Some(scope: ScopeEnvironment) => {
          interfaces.flatMap(m => {
            scope.lookup(EnvironmentLookup.lookupFromName(m.name)) match {
              case Some(id: InterfaceDeclaration) => id.instanceOfList
              case _ => throw new SyntaxError("Could not resolve superclass to find vtable methods.")
            }
          })
        }
        case _ => throw new SyntaxError("Could not find scope environment to get vtable methods.")
      }

      Seq[String](symbolName) ++ interfaceNames
    }
  }

  abstract class ClassMemberDeclaration(
    name: Name,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type
  ) extends ClassBodyDeclaration(name, modifiers) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType)
  }

  case class ConstructorDeclaration(
    override val name: MethodName,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassBodyDeclaration(name, modifiers) with Referenceable with MethodOrConstructorDeclaration {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList
    override def symbolName: String = {
      (
        scope.get.getEnclosingClassNode.get.asInstanceOf[ClassDeclaration].name.niceName
          + "__constructor__"
          + name.niceName
          + parameters.map(_.varType.symbolName).mkString("_")
          + "_" + modifiers.map(_.symbolName).mkString("_")
        ).replaceAll("""^\s+(?m)""", "")
    }
    def isProtected: Boolean = modifiers.contains(ProtectedKeyword())
  }

  sealed trait TypeMethodDeclaration extends AbstractSyntaxNode {
    val modifiers: Set[Modifier]
    val memberType: Type
    val parameters: Seq[FormalParameter]

    def niceName: String
    def isStatic: Boolean = modifiers.contains(StaticKeyword())
    def isProtected: Boolean = modifiers.contains(ProtectedKeyword())
    def symbolName: String
  }

  case class MethodDeclaration(
    override val name: MethodName,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    parameters: Seq[FormalParameter] = Seq.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) with TypeMethodDeclaration with Referenceable with MethodOrConstructorDeclaration {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ parameters.toList ++ body.toList

    override def niceName: String = name.niceName
    override def symbolName: String = {
      (
        scope.get.getEnclosingClassNode.get.asInstanceOf[ClassDeclaration].name.niceName
        + "__"
        + name.niceName
        + parameters.map(_.varType.symbolName).mkString("_")
        + "_" + modifiers.map(_.symbolName).mkString("_")
      ).replaceAll("""^\s+(?m)""", "")
    }
  }

  case class FieldDeclaration(
    override val name: ExpressionName,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    expression: Option[Expression] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) with Referenceable {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ expression.toList

    override def symbolName: String = s"${name.niceName}_0x${hashCode.toHexString}"

    def isStatic: Boolean = modifiers.contains(StaticKeyword())
    def isProtected: Boolean = modifiers.contains(ProtectedKeyword())
  }

  sealed trait BlockStatement extends AbstractSyntaxNode

  sealed trait StackAllocatedVariable extends AbstractSyntaxNode

  case class LocalVariableDeclaration(
    name: ExpressionName,
    memberType: Type,
    expression: Expression
  ) extends BlockStatement with Referenceable with StackAllocatedVariable {
    override def children: List[AbstractSyntaxNode] = List(memberType) ++ List(expression)
    override def symbolName: String = ("LocalVariableDeclaration_" + name.symbolName + "_0x" + hashCode.toHexString).replaceAll("""^\s+(?m)""", "")

  }

  case class Block(statements: Seq[BlockStatement] = Seq.empty[BlockStatement]) extends Statement {
    override def children: List[AbstractSyntaxNode] = statements.toList
  }

  case class CastExpression(targetType: Type, expr: Expression) extends Expression {
    override def children: List[AbstractSyntaxNode] = List(targetType, expr)
  }

  sealed trait Statement extends BlockStatement
  case class EmptyStatement() extends Statement

  sealed trait ForInit extends AbstractSyntaxNode
  sealed trait StatementExpression extends Statement with ForInit with Expression

  case class ReturnStatement(expression: Option[Expression] = None) extends Statement {
    override def children: List[AbstractSyntaxNode] = expression.toList
  }

  case class IfStatement(clause: Expression, trueCase: Option[Statement], elseCase: Option[Statement] = None) extends Statement  {
    override def children: List[AbstractSyntaxNode] = List(clause) ++ trueCase.toList ++ elseCase.toList
    override def symbolName: String = ("If_0x" + hashCode.toHexString)
  }
  case class WhileStatement(clause: Expression, body: Option[Statement]) extends Statement {
    override def children: List[AbstractSyntaxNode] = List(clause) ++ body.toList
    override def symbolName: String = ("While_0x" + hashCode.toHexString)
  }

  case class ForVariableDeclaration(typeDeclaration: Type,
                                    variableName: ExpressionName,
                                    expression: Expression) extends ForInit with Referenceable with StackAllocatedVariable {
    override def children: List[AbstractSyntaxNode] = List(typeDeclaration, variableName, expression)
    override def symbolName: String = ("ForVariableDeclaration_" + variableName.symbolName + "_0x" + hashCode.toHexString).replaceAll("""^\s+(?m)""", "")
  }
  case class ForStatement(init: Option[ForInit],
                          check: Option[Expression],
                          update: Option[StatementExpression],
                          statement: Statement) extends Statement {
    override def children: List[AbstractSyntaxNode] = init.toList ++ check.toList ++ update.toList ++ List(statement)
    override def symbolName: String = ("For_0x" + hashCode.toHexString)
  }

  /*
    Parse!
  */

  def parse(node: ParseNode): Seq[AbstractSyntaxNode] = {
    val result = fromParseNode(Nil)(node)
    result.foreach { overflowCheck }
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
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)

      // The grammar guarantees we should only get one PackageDeclaration,
      // so there's no need to check for multiple here.
      val packageDeclaration: Option[PackageDeclaration] = children.collectFirst { case x: PackageDeclaration => x }
      val importDeclarations: Seq[ImportDeclaration] = children.collect { case x: ImportDeclaration => x }
      val typeDeclarations: Seq[TypeDeclaration] = children.collect { case x: TypeDeclaration => x }

      // Grammar does not guarantee that we only have one type declaration, so make the check here.
      typeDeclarations match {
        case Seq(decl: TypeDeclaration) => {
          val cu = CompilationUnit(packageDeclaration, importDeclarations, decl)
          decl.parent = Some(cu)
          Seq(cu)
        }
        case Seq() => throw new SyntaxError("Must define a class or interface in a file.")
        case _ => throw new SyntaxError("Cannot define more than one class or interface in a file.")
      }

    }

    case p: ParseNodes.PackageDeclaration => {
      val children: Seq[AbstractSyntaxNode] = p.children.flatMap(recursive)
      Seq(PackageDeclaration(children.collectFirst { case x: QualifiedName => x.toPackageName }.get))
    }

    case i: ParseNodes.SingleTypeImportDeclaration => {
      val children: Seq[AbstractSyntaxNode] = i.children.flatMap(recursive)
      Seq(SingleTypeImportDeclaration(children.collectFirst { case x: QualifiedName => x.toTypeName }.get))
    }

    case i: ParseNodes.TypeImportOnDemandDeclaration => {
      val children: Seq[AbstractSyntaxNode] = i.children.flatMap(recursive)
      Seq(TypeImportOnDemandDeclaration(children.collectFirst { case x: QualifiedName => x.toPackageName }.get))
    }

    case c: ParseNodes.ClassDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)
      val name: TypeName = TypeName(children.collectFirst { case x: Identifier => x }.get.value)

      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val superclass: Option[ClassType] = children.collectFirst { case x: ClassType => x }
      val interfaces: Seq[InterfaceType] = children.collect { case x: InterfaceType => x }.toSeq
      val body: ClassBody = children.collectFirst { case x: ClassBody => x }.get

      // Enforce "A class cannot be both abstract and final."
      if (modifiers.collectFirst{case AbstractKeyword() => true}.isDefined && modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
        throw new SyntaxError("Class " + name.value + " cannot be both abstract and final.")
      }

      // Enforce: No package private classes
      if (modifiers.collectFirst{case PublicKeyword() => true}.isEmpty && modifiers.collectFirst{case ProtectedKeyword() => true}.isEmpty) {
        throw new SyntaxError("Class " + name.value + " cannot be package private.")
      }

      // Enforce: Class must match filename
      val filename = name.value.filename.split("/").last
      if (filename != name.value.value + ".java") {
        if (!filename.contains("<input>")) {
          throw new SyntaxError("Class " + name.value + " must be in a file with the same name.")
        }
      }

      Seq(ClassDeclaration(name, body, modifiers, superclass, interfaces))
    }

    case c: ParseNodes.InterfaceDeclaration => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers:Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val interfaces:Seq[InterfaceType] = children.collect { case x: InterfaceType => x }.toSeq
      val body:InterfaceBody = children.collectFirst { case x: InterfaceBody => x }.get

      // Enforce: No package private interface
      if (modifiers.collectFirst{case PublicKeyword() => true}.isEmpty && modifiers.collectFirst{case ProtectedKeyword() => true}.isEmpty) {
        throw new SyntaxError("Interface " + name.value + " cannot be package private.")
      }

      // Enforce: Interface must match filename
      val filename = name.value.filename.split("/").last
      if (filename != name.value.value + ".java") {
        if (!filename.contains("<input>")) {
          throw new SyntaxError("Interface " + name.value + " must be in a file with the same name.")
        }
      }

      Seq(InterfaceDeclaration(TypeName(name.value), body, modifiers, interfaces))
    }

    case c: ParseNodes.ConstructorDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)

      val name: MethodName = children.collectFirst { case x: QualifiedName => x.toMethodName }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val parameters: Seq[FormalParameter] = children.collect { case x: FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x: Block => x }

      // Enforce: No package private methods
      if (modifiers.collectFirst{case PublicKeyword() => true}.isEmpty && modifiers.collectFirst{case ProtectedKeyword() => true}.isEmpty) {
        throw new SyntaxError("Constructor " + name.value + " cannot be package private.")
      }

      // Enforce: A constructor can't be abstract, static, nor final.
      if (modifiers.collectFirst{case AbstractKeyword() => true}.isDefined) {
        throw new SyntaxError("Constructor " + name.value + " cannot be abstract.")
      }

      if (modifiers.collectFirst{case StaticKeyword() => true}.isDefined) {
        throw new SyntaxError("Constructor " + name.value + " cannot be static.")
      }

      if (modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
        throw new SyntaxError("Constructor " + name.value + " cannot be final.")
      }

      // Enforce: A constructor has to have a body.
      if (body == None) {
        throw new SyntaxError("Constructor " + name.value + " has to have a body.")
      }

      Seq(ConstructorDeclaration(name, modifiers, parameters, body))
    }


    case c: ParseNodes.MethodDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)

      val name: MethodName = children
        .collectFirst { case x: Identifier => x }
        .map { identifier: Identifier => MethodName(identifier.value) }
        .get

      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x:Type => x }.get
      val parameters: Seq[FormalParameter] = children.collect { case x:FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x:Block => x }

      // Enforce: No package private methods
      if (!modifiers.collectFirst{case PublicKeyword() => true}.isDefined && !modifiers.collectFirst{case ProtectedKeyword() => true}.isDefined) {
        throw new SyntaxError("Method " + name.niceName + " cannot be package private.")
      }

      // Enforce: A static method cannot be final.
      if (modifiers.collectFirst{case StaticKeyword() => true}.isDefined && modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
        throw new SyntaxError("Method " + name.niceName + " cannot be both static and final.")
      }

      // Enforce: A native method must be static.
      if (modifiers.collectFirst{case NativeKeyword() => true}.isDefined && !modifiers.collectFirst{case StaticKeyword() => true}.isDefined) {
        throw new SyntaxError("Method " + name.niceName + " is native so it must be static.")
      }

      //Enforce: An abstract method cannot be static or final.
      if (modifiers.collectFirst{case AbstractKeyword() => true}.isDefined) {
        if (modifiers.collectFirst{case StaticKeyword() => true}.isDefined || modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
          throw new SyntaxError("Method " + name.niceName + " is abstract so it cant be static or final.")
        }
      }

      // Enforce: A method has a body if and only if it is neither abstract nor native.
      if (modifiers.collectFirst{case NativeKeyword() => true}.isDefined || modifiers.collectFirst{case AbstractKeyword() => true}.isDefined) {
        if (!body.isEmpty) {
          throw new SyntaxError("Method " + name.niceName + " cannot have a body.")
        }
      } else {
        if (body.isEmpty) {
          throw new SyntaxError("Method " + name.niceName + " must have a body.")
        }
      }

      Seq(MethodDeclaration(name, modifiers, memberType, parameters, body))
    }

    case c: ParseNodes.FieldDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)

      val name: ExpressionName = children
        .collectFirst { case x: Identifier => x }
        .map { identifier: Identifier => ExpressionName(identifier.value) }
        .get

      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x: Type => x }.get
      val expression: Option[Expression] = children.collectFirst { case x: Expression => x }

      // Enforce: A final field must have an initializer.
      if (modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
        if (expression.isEmpty) {
          throw new SyntaxError("Field " + name.niceName + " is final so it must have an initializer.")
        }
        memberType.writeable = false
      }

      // Enfore: A field can not be package private
      if (!modifiers.collectFirst{case PublicKeyword() => true}.isDefined && !modifiers.collectFirst{case ProtectedKeyword() => true}.isDefined) {
        throw new SyntaxError("Field " + name.niceName + " cannot be package private.")
      }

      // Enforce: A field can not be abstract
      if (modifiers.collectFirst{case AbstractKeyword() => true}.isDefined) {
        throw new SyntaxError("Field " + name.niceName + " cannot be abstract.")
      }

      // Enforce: A field can not be native
      if (modifiers.collectFirst{case NativeKeyword() => true}.isDefined) {
        throw new SyntaxError("Field " + name.niceName + " cannot be native.")
      }

      Seq(FieldDeclaration(name, modifiers, memberType, expression))
    }

    case c: ParseNodes.InterfaceMemberDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)
      val name: MethodName = children
        .collectFirst { case x: Identifier => x }
        .map { identifier: Identifier => MethodName(identifier.value) }
        .get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x: Type => x }.get
      val parameters: Seq[FormalParameter] = children.collect { case x: FormalParameter => x }
      val body: Option[Block] = children.collectFirst { case x: Block => x }

      // Enforce: An interface method cannot be static or final.
      if (modifiers.collectFirst{case StaticKeyword() => true}.isDefined || modifiers.collectFirst{case FinalKeyword() => true}.isDefined) {
        throw new SyntaxError("Interface method " + name.value + " cannot be static or final.")
      }

      Seq(InterfaceMemberDeclaration(name, modifiers, memberType, parameters, body))
    }

    case i: ParseNodes.Identifier => Seq(Identifier(i.value.get))

    case c: ParseNodes.ClassBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)
      val classBodyDeclarations: Seq[ClassBodyDeclaration] = children.collect { case x: ClassBodyDeclaration => x }

      val constructor: Seq[ConstructorDeclaration] = classBodyDeclarations.collect { case x: ConstructorDeclaration => x }
      if (constructor.isEmpty) {
        throw new SyntaxError("Constructor is required in class body.");
      }

      Seq(ClassBody(classBodyDeclarations))
    }

    case c: ParseNodes.InterfaceBody => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive)
      Seq(InterfaceBody(children.collect { case x: InterfaceMemberDeclaration => x }))
    }

    case m: ParseNodes.StaticKeyword => Seq(StaticKeyword())
    case m: ParseNodes.PublicKeyword => Seq(PublicKeyword())
    case m: ParseNodes.ProtectedKeyword => Seq(ProtectedKeyword())
    case m: ParseNodes.AbstractKeyword => Seq(AbstractKeyword())
    case m: ParseNodes.FinalKeyword => Seq(FinalKeyword())
    case m: ParseNodes.NativeKeyword => Seq(NativeKeyword())

    case c: ParseNodes.CastExpression => {
      val (castNode, expressionNodes) = c.children match {
        case List(
          _: ParseNodes.LeftParen, primitive: ParseNodes.PrimitiveType, _: ParseNodes.RightParen,
          expr: ParseNodes.UnaryExpression
        ) =>
          (recursive(primitive).head, recursive(expr))
        case List(
          _: ParseNodes.LeftParen, primitive: ParseNodes.PrimitiveType, _: ParseNodes.Dims, _: ParseNodes.RightParen,
          expr: ParseNodes.UnaryExpression
        ) =>
          val primitiveType: PrimitiveType = recursive(primitive).collectFirst { case t: PrimitiveType => t }.get
          (ArrayType(primitiveType), recursive(expr))
        case List(
          _: ParseNodes.LeftParen, castExpr: ParseNodes.Expression, _: ParseNodes.RightParen,
          expr: ParseNodes.UnaryExpressionNotPlusMinus
        ) =>
          val exprNode = recursive(castExpr).head
          (exprNode, recursive(expr))
        case List(
          _: ParseNodes.LeftParen, name: ParseNodes.Name, _: ParseNodes.Dims, _: ParseNodes.RightParen,
          expr: ParseNodes.UnaryExpressionNotPlusMinus
        ) =>
          val nameType: Type = recursive(name).head match {
            case name: QualifiedName => ClassOrInterfaceType(name.toTypeName)
            case x => throw new SyntaxError("Casting with invalid cast type: " + x)
          }

          (ArrayType(nameType), recursive(expr))
        case _ => throw new SyntaxError("Invalid cast expression parse: " + c)
      }

      val cast: Type = castNode match {
        // UnaryExpression will give us an ExpressionName, but we want a TypeName
        case y: ExpressionName => ClassOrInterfaceType(y.toQualifiedName.toTypeName)
        case y: QualifiedName => ClassOrInterfaceType(y.toTypeName)
        case y: PrimitiveType => y
        case arr: ArrayType => arr
        case x => throw new SyntaxError("Casting with invalid cast type: " + x)
      }

      val expression: Expression = expressionNodes.head match {
        case name: QualifiedName => name.toExpressionName
        case expr: Expression => expr
        case other => throw new SyntaxError("Right hand side of cast is not an expression: " + other)
      }

      Seq(CastExpression(cast, expression))
    }

    case t: ParseNodes.ClassOrInterfaceType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive)
      Seq(ClassOrInterfaceType(children.head.asInstanceOf[QualifiedName].toTypeName))
    }
    case t: ParseNodes.ClassType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive)
      val child = children.collectFirst { case x: ClassOrInterfaceType => x }.get
      Seq(ClassType(child.name))
    }
    case t: ParseNodes.InterfaceType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive)
      val child = children.collectFirst { case x: ClassOrInterfaceType => x }.get
      Seq(InterfaceType(child.name))
    }

    case i: ParseNodes.Num => {
      val input: InputString = i.value.get
      val num = Num(input.value, input)
      Seq(num)
    }

    case u: ParseNodes.UnaryExpression => {
      val children = u.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })

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

    case u: ParseNodes.UnaryExpressionNotPlusMinus => {
      val children = u.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })

      u.children match {
        case Seq(minus: ParseNodes.LogicalNot, expr: ParseNodes.UnaryExpression) =>
          val child: Seq[AbstractSyntaxNode] = recursive(expr)
          if (child.isEmpty) {
            val expression = children.collectFirst { case x: Expression => x }.get
            Seq(LogicalNotExpression(expression))
          } else {
            child.head match {
              case expr: Expression => Seq(LogicalNotExpression(expr))

              //  If we can't find a Expression, back up and try to parse.
              case _ => child match {
                case Seq(expr: Expression) => Seq(LogicalNotExpression(expr))
                case _ => throw new SyntaxError("Complemented unary expression is malformed: " + child)
              }
            }
          }
        case _ => children
      }
    }

    case t: ParseNodes.TrueLiteral => Seq(TrueLiteral())
    case t: ParseNodes.FalseLiteral => Seq(FalseLiteral())
    case n: ParseNodes.NullLiteral => Seq(NullLiteral())

    case t: ParseNodes.ByteKeyword => Seq(ByteKeyword())
    case t: ParseNodes.ShortKeyword => Seq(ShortKeyword())
    case t: ParseNodes.IntKeyword => Seq(IntKeyword())
    case t: ParseNodes.CharKeyword => Seq(CharKeyword())
    case v: ParseNodes.VoidKeyword => Seq(VoidKeyword())
    case b: ParseNodes.BooleanKeyword => Seq(BooleanKeyword())
    case a: ParseNodes.ArrayType => {
      val children:Seq[AbstractSyntaxNode] = a.children.flatMap(recursive)
      children.headOption match {
        case Some(x: Type) => Seq(ArrayType(x))
        case Some(qn: QualifiedName) => Seq(ArrayType(ClassOrInterfaceType(qn.toTypeName)))
        case _ => throw new SyntaxError("Array type contains non-type node.")
      }
    }
    case p: ParseNodes.FormalParameter => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive)

      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val varType: Type = children.collectFirst { case x: Type => x }.get

      Seq(FormalParameter(ExpressionName(name.value), varType))
    }

    case c: ParseNodes.CharLiteral => Seq(CharLiteral(c.value.get))
    case s: ParseNodes.StringLiteral => Seq(StringLiteral(s.value.get))

    case s: ParseNodes.SimpleName => Seq(QualifiedName(Seq(s.children(0).value.get)))
    case s: ParseNodes.QualifiedName =>
      Seq(QualifiedName(s.children.flatMap(recursive).flatMap {
        case n: Identifier => Some(n.value)
        case n: QualifiedName => n.value
        case _ => throw new SyntaxError("Qualified name contains non-identifiers.")
      }))

    case b: ParseNodes.Block => {
      val children: Seq[AbstractSyntaxNode] = b.children.flatMap(recursive)
      val blockStatements: Seq[BlockStatement] = children.collect { case x: BlockStatement => x }
      Seq(Block(blockStatements))
    }

    case b: ParseNodes.ConstructorBody => {
      val children: Seq[AbstractSyntaxNode] = b.children.flatMap(recursive)
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
      val children:Seq[AbstractSyntaxNode] = l.children.flatMap(recursive)

      val name:Identifier = children.collectFirst { case x: Identifier => x }.get
      val memberType:Type = children.collectFirst { case x: Type => x }.get
      val expression:Expression = children.collectFirst { case x:Expression => x }.get

      Seq(LocalVariableDeclaration(ExpressionName(name.value), memberType, expression))
    }

    case i: ParseNodes.IfThenStatement => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive)
      val thenCase:Option[Statement] = children.collect { case x: Statement => x}.headOption
      Seq(IfStatement(children(0).asInstanceOf[Expression], thenCase))
    }

    case i: ParseNodes.IfThenElseStatement => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive)
      val clauses:Seq[Statement] = children.collect { case x: Statement => x }
      val ifClause:Option[Statement] = clauses.headOption
      val elseClause:Option[Statement] = clauses.drop(1).headOption
      Seq(IfStatement(children(0).asInstanceOf[Expression], ifClause, elseClause))
    }

    case i: ParseNodes.IfThenElseStatementNoShortIf => {
      val children:Seq[AbstractSyntaxNode] = i.children.flatMap(recursive)
      val clauses:Seq[Statement] = children.collect { case x: Statement => x }
      val ifClause:Option[Statement] = clauses.headOption
      val elseClause:Option[Statement] = clauses.drop(1).headOption
      Seq(IfStatement(children(0).asInstanceOf[Expression], ifClause, elseClause))
    }

    case w: ParseNodes.WhileStatement => {
      val children:Seq[AbstractSyntaxNode] = w.children.flatMap(recursive)
      val body:Option[Statement] = children.collect { case x: Statement => x}.headOption
      Seq(WhileStatement(children(0).asInstanceOf[Expression], body))
    }

    case f: ParseNodes.ForStatement => {
      val tokens : Seq[Seq[ParseNode]] = f.children.dropRight(1).foldLeft(Seq(Seq.empty[ParseNode])) {
        (acc, i) =>
          i match {
            case ParseNodes.Semicolon(_,_) => acc :+ Seq.empty
            case _ => acc.init :+ (acc.last :+ i)
          }
      }
      val init: Option[ForInit] = tokens(0).flatMap(recursive).collectFirst { case x: ForInit => x }
      val check: Option[Expression] = tokens(1).flatMap(recursive).collectFirst { case x: Expression => x}

      var statements: Seq[Statement] = tokens(2).flatMap(recursive).collect { case x: Statement => x }
      val update: Option[StatementExpression] = statements.collectFirst { case x: StatementExpression => x }
      if (!update.isEmpty) {
        statements = statements diff List(update.get)
      }

      val statement: Statement = List(f.children.last).flatMap(recursive).collectFirst { case x: Statement => x }.get
      Seq(ForStatement(init, check, update, statement))
    }

    case f: ParseNodes.ForStatementNoShortIf => {
      val tokens : Seq[Seq[ParseNode]] = f.children.dropRight(1).foldLeft(Seq(Seq.empty[ParseNode])) {
        (acc, i) =>
          i match {
            case ParseNodes.Semicolon(_,_) => acc :+ Seq.empty
            case _ => acc.init :+ (acc.last :+ i)
          }
      }
      val init: Option[ForInit] = tokens(0).flatMap(recursive).collectFirst { case x: ForInit => x }
      val check: Option[Expression] = tokens(1).flatMap(recursive).collectFirst { case x: Expression => x}

      var statements: Seq[Statement] = tokens(2).flatMap(recursive).collect { case x: Statement => x }
      val update: Option[StatementExpression] = statements.collectFirst { case x: StatementExpression => x }
      if (!update.isEmpty) {
        statements = statements diff List(update.get)
      }

      val statement: Statement = List(f.children.last).flatMap(recursive).collectFirst { case x: Statement => x }.get
      Seq(ForStatement(init, check, update, statement))
    }

    case f: ParseNodes.ForInit => {
      f.children match {
        case Seq(t: ParseNodes.Type, v: ParseNodes.VariableDeclarator) => {
          val vChildren = v.children.flatMap(recursive)

          val memberType: Type = t.children.flatMap(recursive).collectFirst { case x: Type => x }.get
          val name: Identifier = vChildren.collectFirst { case x: Identifier => x }.get
          val expression: Expression = vChildren.collectFirst { case x: Expression => x }.get

          Seq(ForVariableDeclaration(memberType, ExpressionName(name.value), expression))
        }
        case _ => f.children.flatMap(recursive)
      }
    }

    case r: ParseNodes.ReturnStatement => {
      val children = r.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
      val expression: Option[Expression] = children.collectFirst { case x: Expression => x }
      Seq(ReturnStatement(expression))
    }


    case e: ParseNodes.ConditionalOrExpression => {
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })

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
      val children = e.children.flatMap(recursive).map(_ match {
        case q: QualifiedName => q.toExpressionName
        case a: AbstractSyntaxNode => a
      })
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

    //case e: ParseNodes.AssignmentExpression => Seq(AssignmentExpression(e.children.flatMap(recursive)))

    case e: ParseNodes.Assignment => {
      e.children match {
        case Seq(l: ParseNodes.LeftHandSide, _, r: ParseNodes.AssignmentExpression) => {
          val leftChildren = l.children.flatMap(recursive)
          val rightChildren = r.children.flatMap(recursive)

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
        case _ => e.children.flatMap(recursive)
      }
    }

    case a: ParseNodes.FieldAccess => {
      val children:Seq[AbstractSyntaxNode] = a.children.flatMap(recursive)

      val primary:Primary = children.collectFirst { case x: Primary => x }.get
      val name:Identifier = children.collectFirst { case x: Identifier => x }.get

      Seq(FieldAccess(primary, name.value))
    }
    case a: ParseNodes.ArrayAccess => {
      val children = a.children.flatMap(recursive)
      a.children match {
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftBracket, e: ParseNodes.Expression, r: ParseNodes.RightBracket) => {
          val name: ExpressionName = children.collectFirst { case x: QualifiedName => x.toExpressionName }.get
          val expr: Expression = children.collectFirst { case x: Expression => x }.get
          Seq(SimpleArrayAccess(name, expr))
        }
        case Seq(p: ParseNodes.PrimaryNoNewArray, l: ParseNodes.LeftBracket, e: ParseNodes.Expression, r: ParseNodes.RightBracket) => {
          val pChildren = p.children.flatMap(recursive)
          val eChildren = e.children.flatMap(recursive)
          val primary: Primary = pChildren.collectFirst { case x: Primary => x }.get
          val expr: Expression = eChildren.collectFirst { case x: Expression => x }.get
          Seq(ComplexArrayAccess(primary, expr))
        }
        case _ => a.children.flatMap(recursive)
      }
    }

    case ParseNodes.PrimaryNoNewArray(List(lp: ParseNodes.LeftParen, content: ParseNode, rp: ParseNodes.RightParen), _) =>
      Seq(ParenthesizedExpression(content.children.flatMap(recursive).collectFirst {case x: Expression => x}.get))

    case t: ParseNodes.ThisKeyword => Seq(ThisKeyword())

    case p: ParseNodes.StatementExpression => {
      val children: Seq[AbstractSyntaxNode] = p.children.flatMap(recursive)
      val expression: Expression = children.collectFirst { case x: Expression => x }.get

      expression match {
        case node: ClassCreationPrimary => Seq(ClassCreationStatementExpression(node))
        case node: MethodInvocation => Seq(MethodInvocationExpression(node))
        case _ => children
      }
    }

    case p: ParseNodes.ArrayCreationExpression => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive)

      val varType:Type = children.collectFirst { case x: Type => x }.get
      val dimExpr:Expression = children.collectFirst { case x: Expression => x }.get

      Seq(ArrayCreationPrimary(varType, dimExpr))
    }

    case p: ParseNodes.ClassInstanceCreationExpression => {
      val children:Seq[AbstractSyntaxNode] = p.children.flatMap(recursive)

      val classType:ClassType = children.collectFirst { case x: ClassType => x }.get
      val args:Seq[Expression] = children.collect { case x: Expression => x }

      Seq(ClassCreationPrimary(classType, args))
    }

    case m: ParseNodes.MethodInvocation => {
      val children = m.children.flatMap(recursive)
      m.children match {
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftParen, a: ParseNodes.ArgumentList, r: ParseNodes.RightParen) => {
          val name: MethodName = children.collectFirst { case x: QualifiedName => x.toMethodName }.get
          val args: Seq[Expression] = children.drop(1).collect { case x: Expression => x }
          Seq(SimpleMethodInvocation(name, args))
        }
        case Seq(n: ParseNodes.Name, l: ParseNodes.LeftParen, r: ParseNodes.RightParen) => {
          val name: MethodName = children.collectFirst { case x: QualifiedName => x.toMethodName }.get
          Seq(SimpleMethodInvocation(name))
        }
        case Seq(p: ParseNodes.Primary, d: ParseNodes.Dot, i: ParseNodes.Identifier, l: ParseNodes.LeftParen, e: ParseNodes.ArgumentList, r: ParseNodes.RightParen) => {
          val primary: Primary = children.collectFirst { case x: Primary => x }.get
          val args: Seq[Expression] = e.children.flatMap(recursive).collect { case x: Expression => x }
          val name: MethodName = children.collectFirst {
            case x: QualifiedName => x.toMethodName
            case x: Identifier => MethodName(x.value)
          }.get
          Seq(ComplexMethodInvocation(primary, name, args))
        }
        case Seq(p: ParseNodes.Primary, d: ParseNodes.Dot, i: ParseNodes.Identifier, l: ParseNodes.LeftParen, r: ParseNodes.RightParen) => {
          val primary: Primary = children.collectFirst { case x: Primary => x }.get
          val name: MethodName = children.collectFirst {
            case x: QualifiedName => x.toMethodName
            case x: Identifier => MethodName(x.value)
          }.get
          Seq(ComplexMethodInvocation(primary, name))
        }
        case _ => m.children.flatMap(recursive)
      }
    }

    // If the parse node does not map nicely to an ASN, just hand us its children.
    case p: ParseNode => p.children.flatMap(recursive)
  }
  }
}
