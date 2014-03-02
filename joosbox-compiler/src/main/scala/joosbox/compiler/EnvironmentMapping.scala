package joosbox.compiler

import joosbox.parser.{
  AbstractSyntaxNode
}

class EnvironmentMapping(
  val environment: Environment,
  val mapping: Map[AbstractSyntaxNode, Environment]
) {
}