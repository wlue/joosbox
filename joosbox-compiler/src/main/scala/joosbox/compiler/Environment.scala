package joosbox.compiler

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  Name,
  Referenceable
}

sealed trait Environment {
  val parent: Option[Environment]
  def lookup(name: Name): Option[Referenceable]
}

class CompilationEnvironment extends Environment {
  val parent: Option[Environment] = None
  def lookup(name: Name): Option[Referenceable] = None
}
