package joosbox.compiler

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.CompilationUnit

object EnvironmentBuilder {
  def build(
    nodes: Seq[AbstractSyntaxNode.CompilationUnit]
  ): EnvironmentMapping = {
    new EnvironmentMapping(
      new CompilationEnvironment,
      Map.empty
    )
  }
}
