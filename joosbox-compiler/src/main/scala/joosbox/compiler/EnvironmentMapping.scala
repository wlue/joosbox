package joosbox.compiler

import joosbox.parser.{
  AbstractSyntaxNode
}

class EnvironmentMapping(
  val environment: Environment,
  val mapping: Map[AbstractSyntaxNode, Environment]
) {
  def enclosingScopeOf(asn: AbstractSyntaxNode): Option[Environment] = mapping.get(asn)

  def validateEnvironments = {
    mapping.foreach { case (ast, env) =>
      env match {
        case s : ScopeEnvironment =>
          s.useLinkedScopes = true
        case _ => Unit
      }
    }
  }
}
