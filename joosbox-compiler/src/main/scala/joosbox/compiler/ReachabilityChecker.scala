package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import joosbox.parser._

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object ReachabilityChecker {
  import AbstractSyntaxNode._

  def link(units: Seq[CompilationUnit]): Map[Any, Referenceable] = {
    units.foreach { unit => TypeChecker.check(unit) }
    Map.empty
  }

  def check(node: AbstractSyntaxNode) {
    node.children.foreach { node => check(node) }
  }
}
