package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object TypeChecker {
  import AbstractSyntaxNode._

  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      TypeChecker.check(unit)(mapping)
    }
    Map.empty
  }

  def resolveType(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) : Option[Type] = {
    node match {
      case TrueLiteral => Some(BooleanKeyword)
      case FalseLiteral => Some(BooleanKeyword)
      case VoidKeyword => Some(VoidKeyword)
      case x: StringLiteral =>
        Some(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName))
      case c: CastExpression => Some(c.targetType)
      case e: ExpressionName =>
        val env = mapping.enclosingScopeOf(node).get
        val lookup = ExpressionNameLookup(e)
        val ref = env.lookup(lookup).get
        ref match {
          case p : FormalParameter => Some(p.varType)
          case f : FieldDeclaration => Some(f.memberType)
          case v : LocalVariableDeclaration => Some(v.memberType)
          case v : ForVariableDeclaration => Some(v.typeDeclaration)
          case _ => throw new SyntaxError("Could not resolve type of expression.");
        }
      case _ => None
    }
  }

  def checkLogicalExpression(e1: Expression, e2: Expression)(implicit mapping: EnvironmentMapping) = {
    resolveType(e1) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
    resolveType(e2) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
      case e: AndExpression =>
        Unit //TypeChecker.checkLogicalExpression(e.e1, e.e2)
      case e: OrExpression =>
        Unit //TypeChecker.checkLogicalExpression(e.e1, e.e2)
      case c : ClassCreationPrimary =>
        val classType : ClassType = c.classType
        val env = mapping.enclosingScopeOf(node).get
        val ref = env.lookup(TypeNameLookup(classType.name)).get
        ref match {
          case klass: ClassDeclaration =>
            if (klass.modifiers.contains(AbstractKeyword)) {
              throw new SyntaxError("Can't create instances of an abstract class.")
            }
          case _ => Unit
        }

      case _ => Unit
    }
    node.children.foreach { node => check(node) }
  }
}
