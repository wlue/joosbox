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

  def resolveType(node: AbstractSyntaxNode) : Option[Type] = {
    node match {
      case TrueLiteral => Some(BooleanKeyword)
      case FalseLiteral => Some(BooleanKeyword)
      case VoidKeyword => Some(VoidKeyword)
      case x: StringLiteral =>
        Some(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName))
      case c: CastExpression => Some(c.targetType)
      case _ => None
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
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
