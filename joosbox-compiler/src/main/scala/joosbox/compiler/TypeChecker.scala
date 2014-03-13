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

  def resolveType(node: AbstractSyntaxNode) : Type = {
    node match {
      case x: StringLiteral =>
        ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName)
      case c: CastExpression => c.targetType
      case _ => Unit
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {

      case _ => Unit
    }
    node.children.foreach { node => check(node) }
  }
}
