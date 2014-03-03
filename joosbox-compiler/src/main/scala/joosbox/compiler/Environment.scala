package joosbox.compiler

import joosbox.lexer.InputString

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  Name,
  Type,
  Referenceable
}

sealed trait EnvironmentLookup
case class NameLookup(name: InputString) extends EnvironmentLookup
case class MethodLookup(name: InputString, params: Seq[Type]) extends EnvironmentLookup

sealed trait Environment {
  val parent: Option[Environment]

  def lookup(name: EnvironmentLookup): Option[Referenceable] = {
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
  def search(name: EnvironmentLookup): Option[Referenceable]
}


/**
 * Top level environment.
 */
class CompilationEnvironment extends Environment {
  val parent: Option[Environment] = None

  // TODO
  def search(name: EnvironmentLookup): Option[Referenceable] = None
}


/**
 * Environment for file (Compilation Unit in AST).
 */
class FileEnvironment(
  par: CompilationEnvironment
) extends Environment {
  val parent: Option[Environment] = Some(par)

  // TODO
  def search(name: EnvironmentLookup): Option[Referenceable] = None
}

/**
 * Environment for file (Compilation Unit in AST).
 */
class ScopeEnvironment(
  par: Environment,
  members: Map[EnvironmentLookup, Referenceable]
) extends Environment {
  val parent: Option[Environment] = Some(par)
  override def toString(): String = members.toString()

  def search(name: EnvironmentLookup): Option[Referenceable] = members.get(name)
}
