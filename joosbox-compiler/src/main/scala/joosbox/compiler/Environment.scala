package joosbox.compiler

import joosbox.lexer.{
  InputString,
  SyntaxError
}

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
case class IdentifierLookup(identifier: InputString) extends EnvironmentLookup
case class NameLookup(name: InputString) extends EnvironmentLookup
case class QualifiedNameLookup(name: QualifiedName) extends EnvironmentLookup
case class MethodLookup(name: InputString, params: Seq[Type]) extends EnvironmentLookup
case class ConstructorLookup(params: Seq[Type]) extends EnvironmentLookup

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

  /**
   * Get the package scope from the parent. Only the root should respond to this.
   */
  def packageScope(name: QualifiedNameLookup): Seq[ScopeEnvironment] = parent match {
    case Some(p: Environment) => p.packageScope(name)
    case _ => Seq.empty[ScopeEnvironment]
  }
}

/**
 * Top level environment, encompassing multiple files' scopes.
 */
class RootEnvironment(nodes: Seq[AbstractSyntaxNode.CompilationUnit]) extends Environment {
  val parent: Option[Environment] = None

  val qualifiedNameMap: Map[QualifiedName, Referenceable] = {
    nodes.foldLeft(Map.empty[QualifiedName, Referenceable]) {
      case (map: Map[QualifiedName, Referenceable], cu: AbstractSyntaxNode.CompilationUnit) => {
        val declaration: Option[TypeDeclaration] = cu.typeDeclaration
        val mapping = cu.packageDeclaration match {
          case Some(p: PackageDeclaration) => p.name match {
            case s: SimpleName => declaration.map(d => QualifiedName(Seq(s.value, d.name)) -> d)
            case q: QualifiedName => declaration.map(d => QualifiedName(q.value.toSeq ++ Seq(d.name)) -> d)
          }

          case None => declaration.map(d => QualifiedName(Seq(InputString(""), d.name)) -> d)
        }

        mapping match {
          case Some((q: QualifiedName, t: TypeDeclaration)) => {
            map.get(q) match {
              case None => map + (q -> t)
              case Some(_) => throw new SyntaxError("Duplicate qualified name " + q)
            }
          }
          case _ => map
        }
      }
    }
  }

  var packageScopeMap: Map[QualifiedName, Seq[ScopeEnvironment]] = Map.empty[QualifiedName, Seq[ScopeEnvironment]]

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    name match {
      case QualifiedNameLookup(qn: QualifiedName) =>
        qualifiedNameMap.get(qn)

      //  The root environment can only handle qualified name lookups.
      case _ => None
    }
  }

  override def packageScope(name: QualifiedNameLookup): Seq[ScopeEnvironment]
    = packageScopeMap.getOrElse(name.name, Seq.empty[ScopeEnvironment])
}

/**
 * Environment for file or AST node scope (Compilation Unit in AST).
 */
class ScopeEnvironment(
  val locals: Map[EnvironmentLookup, Referenceable],
  val packageScopeReference: Option[QualifiedNameLookup],
  val importScopeReferences: Seq[QualifiedNameLookup],
  par: Environment
) extends Environment {

  val parent: Option[Environment] = Some(par)
  override def toString(): String = super.toString()+"<par: @"+Integer.toHexString(par.hashCode())+">[package: " + packageScopeReference + ", imports: " + importScopeReferences + "](" + locals.toString() + ")"

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    locals.get(name) match {
      case Some(r: Referenceable) => Some(r)

      //  Check our other scope references - our package scopes, then our wildcard imports.
      //  Note that wildcard imports are pretty much just additional package scopes, except
      //  for the fact that package scopes are higher priority than wildcard imports.
      case None => {
        val packageResolved: Option[Referenceable]
          = packageScopeReference.toSeq.flatMap(packageScope(_)).flatMap(env => {
          if (env != this) {
            env.locals.get(name)
          } else {
            None
          }
        }).headOption

        packageResolved match {
          case Some(_) => packageResolved
          case None => {
            val importScopes: Seq[ScopeEnvironment]
              = importScopeReferences.flatMap(packageScope(_))

            val uniqueDeclarations: Map[EnvironmentLookup, AbstractSyntaxNode.Referenceable]
              = importScopes.foldLeft(Map.empty[EnvironmentLookup, AbstractSyntaxNode.Referenceable]) {
                case (map: Map[EnvironmentLookup, AbstractSyntaxNode.Referenceable], env: ScopeEnvironment) => {
                  if (env != this) {
                    env.locals.get(name) match {
                      case Some(ref: AbstractSyntaxNode.Referenceable) => {
                        map.get(name) match {
                          case None => map + (name -> ref)
                          case Some(existing: AbstractSyntaxNode.Referenceable) => {
                            if ((ref == existing) && (ref.parentOption == existing.parentOption)) {
                              map
                            } else {
                              throw new SyntaxError("Multiple conflicting on-demand-imported types found for " + ref)
                            }
                          }
                        }
                      }
                      case None => map
                    }
                  } else {
                    map
                  }
                }
              }

            uniqueDeclarations.get(name)
          }
        }
      }
    }
  }

  def ensureUnambiguousReferences(mapping: Map[AbstractSyntaxNode, Environment]) = {
    val uniqueScopes: Seq[ScopeEnvironment] =
      (packageScopeReference.toSeq ++ importScopeReferences).flatMap(packageScope(_)).toSet[ScopeEnvironment].toSeq

    val uniqueDeclarations = uniqueScopes.flatMap(s => {
      s.locals.keys.flatMap(s.lookup(_))
    })

    //val duplicateTypes = types.groupBy(identity).filter {case (_, l) => l.size > 1 }.keys
    //println(duplicateTypes)
  }
}
