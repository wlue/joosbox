package joosbox.compiler

import joosbox.lexer.{
  InputString,
  SyntaxError
}

import joosbox.parser.AbstractSyntaxNode
import joosbox.parser.AbstractSyntaxNode._

sealed trait EnvironmentLookup

case class PackageNameLookup(name: PackageName) extends EnvironmentLookup
case class TypeNameLookup(name: TypeName) extends EnvironmentLookup
case class ExpressionNameLookup(name: ExpressionName) extends EnvironmentLookup
case class MethodLookup(name: InputString, params: Seq[Type]) extends EnvironmentLookup
case class ConstructorLookup(name: InputString, params: Seq[Type]) extends EnvironmentLookup

object EnvironmentLookup {
  def lookupFromName(name: Name) = name match {
    case p: PackageName => PackageNameLookup(p)
    case t: TypeName => TypeNameLookup(t)
    case e: ExpressionName => ExpressionNameLookup(e)

    case m: MethodName => throw new SyntaxError("MethodName lookups must include parameters.")
    case a: AmbiguousName => throw new SyntaxError("AmbiguousNames cannot be looked up.")

    case s: SimpleName => throw new SyntaxError("SimpleNames cannot be looked up.")
    case q: QualifiedName => throw new SyntaxError("SimpleNames cannot be looked up.")
  }
}

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

  //  To facilitate method lookups without knowledge of their parameter types.
  def lookupMethodsByName(name: InputString): Seq[MethodDeclaration] =
    searchForMethodsWithName(name) ++ (parent match {
      case Some(env: Environment) => env.lookupMethodsByName(name)
      case None => Seq.empty[MethodDeclaration]
    })

  /**
   * Search the current scope for a method with the given name.
   */
  def searchForMethodsWithName(name: InputString): Seq[MethodDeclaration] = Seq.empty[MethodDeclaration]

  /**
   * Search the current scope for the name.
   */
  def search(name: EnvironmentLookup): Option[Referenceable]

  /**
   * Get the package scope from the parent. Only the root should respond to this.
   */
  def packageScope(name: PackageNameLookup): Seq[ScopeEnvironment] = parent match {
    case Some(p: Environment) => p.packageScope(name)
    case _ => Seq.empty[ScopeEnvironment]
  }
}

/**
 * Top level environment, encompassing multiple files' scopes.
 */
class RootEnvironment(nodes: Seq[AbstractSyntaxNode.CompilationUnit]) extends Environment {
  val parent: Option[Environment] = None

  val qualifiedNameMap: Map[TypeName, Referenceable] = {
    nodes.foldLeft(Map.empty[TypeName, Referenceable]) {
      case (map: Map[TypeName, Referenceable], cu: AbstractSyntaxNode.CompilationUnit) => {
        val declaration: Option[TypeDeclaration] = cu.typeDeclaration
        val mapping = cu.packageDeclaration match {
          case Some(p: PackageDeclaration) => declaration.map(d => TypeName(d.name.value, Some(p.name)) -> d)

          case None => declaration.map(d => TypeName(d.name.value, Some(PackageName(InputString("")))) -> d)
        }

        mapping match {
          case Some((q: TypeName, t: TypeDeclaration)) => {
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
  var packageScopeMap: Map[PackageName, Seq[ScopeEnvironment]] = Map.empty[PackageName, Seq[ScopeEnvironment]]

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    name match {
      case TypeNameLookup(qn: TypeName) =>
        qualifiedNameMap.get(qn)

      //  The root environment can only handle qualified name lookups.
      case _ => None
    }
  }

  override def packageScope(name: PackageNameLookup): Seq[ScopeEnvironment]
    = packageScopeMap.getOrElse(name.name, Seq.empty[ScopeEnvironment])
}

/**
 * Environment for file or AST node scope (Compilation Unit in AST).
 */
class ScopeEnvironment(
  val locals: Map[EnvironmentLookup, Referenceable],

  //  For top-level scopepes.
  val packageScopeReference: Option[PackageNameLookup],
  val importScopeReferences: Seq[PackageNameLookup],
  par: Environment,

  //  For classes, how do we find methods and fields in their superclasses/interfaces?
  val linkedScopeReferences: Seq[EnvironmentLookup] = Seq.empty[EnvironmentLookup]
) extends Environment {

  val parent: Option[Environment] = Some(par)
  override def toString(): String = super.toString()+"<par: @"+Integer.toHexString(par.hashCode())+">[package: " + packageScopeReference + ", imports: " + importScopeReferences + "](" + locals.toString() + ")"

  override def searchForMethodsWithName(name: InputString): Seq[MethodDeclaration] = {
    locals.collect{
      case (MethodLookup(methodName: InputString, _), method: MethodDeclaration)
      if (methodName == name) => method
    }.toSeq ++ linkedScopes.flatMap(_.searchForMethodsWithName(name))
  }

  //  This is not very scala-y or functional, but we need this to link class
  //  scopes to the scopes of their superclasses and interfaces.
  var linkedScopes: Seq[ScopeEnvironment] = Seq.empty[ScopeEnvironment]
  def linkScopesWithMapping(mapping: Map[AbstractSyntaxNode, Environment]) = {
    linkedScopes = linkedScopeReferences.flatMap(lookup(_)).flatMap(mapping.get(_)).collect{case c: ScopeEnvironment => c}
  }

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    locals.get(name) match {
      case Some(r: Referenceable) =>
        Some(r)

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

            uniqueDeclarations.get(name) match {
              case Some(x) => Some(x)
              case None => {
                //  Finally, search all of our linked scopes for the name (but not their parents).
                linkedScopes.toStream.flatMap(_.search(name)).headOption match {
                  case Some(r: Referenceable) => {
                    println("Found referenceable when looking in other scope: " + r)
                    None
                  }
                  case None => None
                }
              }
            }
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
