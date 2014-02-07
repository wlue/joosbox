package joosbox.parser

import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

sealed trait AbstractSyntaxNode {
  def children: List[AbstractSyntaxNode] = List.empty[AbstractSyntaxNode]

  def simpleString(indent: Int = 0): String = {
    (" " * indent) + this.getClass.getSimpleName + "\n" + children.map(_.simpleString(indent + 2)).mkString("")
  }
}

object AbstractSyntaxNode {
  case class CharLiteral(value: InputString) extends AbstractSyntaxNode
  case class StringLiteral(value: InputString) extends AbstractSyntaxNode
  case object NullLiteral extends AbstractSyntaxNode

  case class Num(value: String, input: InputString) extends AbstractSyntaxNode {
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
    typeDeclarations: Seq[TypeDeclaration] = Seq.empty[TypeDeclaration]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] =
      packageDeclaration.toList ++ importDeclarations.toList ++ typeDeclarations.toList
  }

  case class PackageDeclaration(name: InputString) extends AbstractSyntaxNode

  sealed trait ImportDeclaration extends AbstractSyntaxNode {
    def name: InputString
  }

  case class SingleTypeImportDeclaration(name: InputString) extends ImportDeclaration
  case class TypeImportOnDemandDeclaration(name: InputString) extends ImportDeclaration

  case class Identifier(value: InputString) extends AbstractSyntaxNode

  abstract class ClassBodyDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier]
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = modifiers.toList
  }

  //  TODO; implement me properly, not just stubbed out
  case class InterfaceMemberDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,

    parameters: Set[FormalParameter] = Set.empty[FormalParameter],
    body: Option[Block] = None
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList
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

  // TODO: Don't do it like this.
  case class Expression(nodes: Seq[AbstractSyntaxNode]) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = nodes.toList
  }

  sealed trait Type extends AbstractSyntaxNode

  sealed trait Name extends Type
  case class SimpleName(value: InputString) extends Name
  case class QualifiedName(value: Seq[InputString]) extends Name

  sealed trait PrimitiveType extends Type
  sealed trait ReferenceType extends Type
  case object VoidKeyword extends Type

  case object BooleanKeyword extends PrimitiveType

  case object TrueLiteral extends AbstractSyntaxNode
  case object FalseLiteral extends AbstractSyntaxNode

  sealed trait NumericType extends PrimitiveType

  case object ByteKeyword extends NumericType
  case object ShortKeyword extends NumericType
  case object IntKeyword extends NumericType
  case object CharKeyword extends NumericType

  abstract class FormalParameter(
    val name: InputString,
    val varType: Type
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = List(varType)
  }

  case class ArrayType(subtype: Type) extends ReferenceType {
    override def children: List[AbstractSyntaxNode] = List(subtype)
  }

  case class ClassOrInterfaceType(name: Name) extends ReferenceType
  case class ClassType(name: Name) extends ReferenceType
  case class InterfaceType(name: Name) extends ReferenceType

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
  ) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ interfaces.toList
  }

  case class ClassDeclaration(
    override val name: InputString,
    body: ClassBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    superclass: Option[ClassType] = None,
    override val interfaces: Set[InterfaceType] = Set.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) {
    override def children: List[AbstractSyntaxNode] =
      List(body) ++ superclass.toList ++ modifiers.toList ++ interfaces.toList
  }

  case class InterfaceDeclaration(
    override val name: InputString,
    body: InterfaceBody,

    override val modifiers: Set[Modifier] = Set.empty[Modifier],
    override val interfaces: Set[InterfaceType] = Set.empty[InterfaceType]
  ) extends TypeDeclaration(name, modifiers, interfaces) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ interfaces.toList
  }

  abstract class ClassMemberDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type
  ) extends ClassBodyDeclaration(name, modifiers) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType)
  }

  case class ConstructorDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    parameters: Set[FormalParameter] = Set.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassBodyDeclaration(name, modifiers) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ parameters.toList ++ body.toList
  }

  case class MethodDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    parameters: Set[FormalParameter] = Set.empty[FormalParameter],
    body: Option[Block] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ parameters.toList ++ body.toList
  }

  case class FieldDeclaration(
    name: InputString,
    modifiers: Set[Modifier] = Set.empty[Modifier],
    memberType: Type,
    expression: Option[Expression] = None
  ) extends ClassMemberDeclaration(name, modifiers, memberType) {
    override def children: List[AbstractSyntaxNode] =
      modifiers.toList ++ List(memberType) ++ expression.toList
  }

  sealed trait BlockStatement extends AbstractSyntaxNode

  case class LocalVariableDeclaration(
    name: InputString,
    memberType: Type,
    expression: Option[Expression] = None
  ) extends BlockStatement {
    override def children: List[AbstractSyntaxNode] = expression.toList
  }

  // TODO: Implement me
  case class Statement(nodes: Seq[AbstractSyntaxNode]) extends BlockStatement {
    override def children: List[AbstractSyntaxNode] = nodes.toList
  }

  case class Block(statements: Seq[BlockStatement]) extends AbstractSyntaxNode {
    override def children: List[AbstractSyntaxNode] = statements.toList
  }

  case class CastExpression() extends AbstractSyntaxNode

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

    node.children.foreach { node => overflowCheck(node) }
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

      Seq(CompilationUnit(packageDeclaration, importDeclarations, typeDeclarations))
    }

    case p: ParseNodes.PackageDeclaration => Seq(PackageDeclaration(p.children(1).value.get))

    case i: ParseNodes.SingleTypeImportDeclaration   => Seq(SingleTypeImportDeclaration(i.children(1).value.get))
    case i: ParseNodes.TypeImportOnDemandDeclaration => Seq(TypeImportOnDemandDeclaration(i.children(1).value.get))

    case c: ParseNodes.ClassDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      val name: Identifier = children.collectFirst { case x: Identifier => x }.get

      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val superclass: Option[ClassType] = children.collectFirst { case x: ClassType => x }
      val interfaces: Set[InterfaceType] = children.collect { case x: InterfaceType => x }.toSet
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

    case c: ParseNodes.ConstructorDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name: SimpleName = children.collectFirst { case x: SimpleName => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val parameters: Set[FormalParameter] = children.collect { case x: FormalParameter => x }.toSet
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

      Seq(ConstructorDeclaration(name.value, modifiers, parameters, body))
    }


    case c: ParseNodes.MethodDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x:Type => x }.get
      val parameters: Set[FormalParameter] = children.collect { case x:FormalParameter => x }.toSet
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

      Seq(FieldDeclaration(name.value, modifiers, memberType, expression))
    }

    case c: ParseNodes.InterfaceMemberDeclaration => {
      val children: Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))
      val name: Identifier = children.collectFirst { case x: Identifier => x }.get
      val modifiers: Set[Modifier] = children.collect { case x: Modifier => x }.toSet
      val memberType: Type = children.collectFirst { case x: Type => x }.get
      val parameters: Set[FormalParameter] = children.collect { case x: FormalParameter => x }.toSet
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

      val constructor: Option[ConstructorDeclaration] = classBodyDeclarations.collectFirst { case x: ConstructorDeclaration => x }
      if (constructor == None) {
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

    // TODO: Fix this hackery 
    case c: ParseNodes.CastExpression => {
      val children:Seq[AbstractSyntaxNode] = c.children.flatMap(recursive(_))

      children.headOption match {
        case Some(x: PrimitiveType) => Seq(CastExpression())
        case Some(x: Expression) => x.children.headOption match {
          case Some(y: SimpleName) => {
            if (x.children.size == 1) {
              Seq(CastExpression())
            } else {
              throw new SyntaxError("Casting with expressions is not supported")
            }
          }
          case _ => throw new SyntaxError("Casting with expressions is not supported")
        }
        case Some(x: SimpleName) => Seq(CastExpression())
        case _ => throw new SyntaxError("Casting requires a valid cast type")
      }
    }

    case t: ParseNodes.ClassType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive(_))
      Seq(ClassType(recursive(t.children.head).head.asInstanceOf[Name]))
    }
    case t: ParseNodes.InterfaceType => {
      val children: Seq[AbstractSyntaxNode] = t.children.flatMap(recursive(_))
      Seq(ClassOrInterfaceType(recursive(t.children.head).head.asInstanceOf[Name]))
    }

    case i: ParseNodes.Num =>
      val input: InputString = i.value.get
      val num = Num(input.value, input)
      Seq(num)

    case u: ParseNodes.UnaryExpression => u.children match {
      case Seq(minus: ParseNodes.Minus, expr: ParseNodes.UnaryExpression) =>
        val parsed: Seq[AbstractSyntaxNode] = recursive(expr)
        parsed match {
          case Seq(num: Num) => Seq(num.negated)
          case _ => parsed
        }
      case _ => u.children.flatMap(recursive(_))
    }

    case t: ParseNodes.TrueLiteral => Seq(TrueLiteral)
    case t: ParseNodes.FalseLiteral => Seq(FalseLiteral)

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

    case s: ParseNodes.SimpleName => Seq(SimpleName(s.children(0).value.get))
    case s: ParseNodes.QualifiedName =>
      Seq(QualifiedName(s.children.flatMap(recursive(_)).flatMap {
        case n: SimpleName => Some(n.value)
        case n: Identifier => Some(n.value)
        case n: QualifiedName => n.value
        case n: AbstractSyntaxNode => throw new SyntaxError("Qualified name contains non-identifiers.")
      }))

    // TODO: Implement
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

    // TODO: Implement
    case s: ParseNodes.Statement => Seq(Statement(s.children.flatMap(recursive(_))))

    // TODO: Implement
    case e: ParseNodes.Expression => Seq(Expression(e.children.flatMap(recursive(_))))

    // If the parse node does not map nicely to an ASN, just hand us its children.
    case p: ParseNode => p.children.flatMap(recursive(_))
  }
  }
}
