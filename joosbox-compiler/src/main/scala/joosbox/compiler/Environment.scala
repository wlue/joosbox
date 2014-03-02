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

  def lookup(name: Name): Option[Referenceable] = {
    // Recursively look up to each parent.
    val result = search(name)
    result match {
      case Some(ref) => Some(ref)
      case None =>
        parent.flatMap { env: Environment => env.lookup(name) }
    }
  }

  /**
   * Search the current scope for the name.
   */
  def search(name: Name): Option[Referenceable]
}


/**
 * Top level environment.
 */
class CompilationEnvironment extends Environment {
  val parent: Option[Environment] = None

  // TODO
  def search(name: Name): Option[Referenceable] = None
}


/**
 * Environment for file (Compilation Unit in AST).
 */
class FileEnvironment(
  par: CompilationEnvironment
) extends Environment {
  val parent: Option[CompilationEnvironment] = Some(par)

  // TODO
  def search(name: Name): Option[Referenceable] = None
}
