package joosbox.compiler

import joosbox.lexer.InputString

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  Name,
  Type,
  Referenceable,
  SimpleName,
  QualifiedName,
  TypeDeclaration,
  PackageDeclaration
}

sealed trait EnvironmentLookup
case class NameLookup(name: InputString) extends EnvironmentLookup
case class QualifiedNameLookup(name: QualifiedName) extends EnvironmentLookup
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
 * Top level environment, encompassing multiple files' scopes.
 */
class RootEnvironment(nodes: Seq[AbstractSyntaxNode.CompilationUnit]) extends Environment {
  val parent: Option[Environment] = None

  val qualifiedNameMap: Map[QualifiedName, Referenceable] = {
    nodes.flatMap(cu => {
      val declarations: Seq[TypeDeclaration] = (cu.interfaceDeclarations ++ cu.classDeclaration)
      cu.packageDeclaration match {
        case Some(p: PackageDeclaration) => p.name match {
          case s: SimpleName => declarations.map(d => QualifiedName(Seq(s.value, d.name)) -> d)
          case q: QualifiedName => declarations.map(d => QualifiedName(q.value.toSeq ++ Seq(d.name)) -> d)
        }

        //  TODO: If we are in the unnamed package, how do other packages access our members?
        case None => declarations.map(d => QualifiedName(Seq(InputString(""), d.name)) -> d)
      }
    }).toMap
  }

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    name match {
      case QualifiedNameLookup(qn: QualifiedName) => qualifiedNameMap.get(qn)

      //  The root environment can only handle qualified name lookups.
      case _ => None
    }
  }
}

/**
 * Environment for file or AST node scope (Compilation Unit in AST).
 */
class ScopeEnvironment(
  par: Environment,
  members: Map[EnvironmentLookup, Referenceable]
) extends Environment {
  val parent: Option[Environment] = Some(par)
  override def toString(): String = members.toString()

  def search(name: EnvironmentLookup): Option[Referenceable] = members.get(name)
}
