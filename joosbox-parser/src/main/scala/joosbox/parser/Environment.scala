package joosbox.parser

import joosbox.lexer.{
  InputString,
  SyntaxError
}

import AbstractSyntaxNode._

//  Looking up a package name does not return us a Referenceable,
//  so let's not call it an EnvironmentLookup.
case class PackageNameLookup(name: PackageName)

sealed trait EnvironmentLookup

case class TypeNameLookup(name: QualifiedName) extends EnvironmentLookup
case class ExpressionNameLookup(name: QualifiedName) extends EnvironmentLookup
case class MethodLookup(name: QualifiedName, params: Seq[Type]) extends EnvironmentLookup
case class ConstructorLookup(name: QualifiedName, params: Seq[Type]) extends EnvironmentLookup

object EnvironmentLookup {
  var enableLinkedScopes: Boolean = false

  def lookupFromName(name: Name) = name match {

    //  Changing the input into a QualifiedName gets rid of the scope.
    case t: TypeName => TypeNameLookup(t.toQualifiedName)
    case e: ExpressionName => ExpressionNameLookup(e.toQualifiedName)

    case m: MethodName => throw new SyntaxError("MethodName lookups must include parameters.")
    case a: AmbiguousName => throw new SyntaxError("AmbiguousNames cannot be looked up.")
    case p: PackageName => throw new SyntaxError("PackageName lookups must use packageScope(), not lookup().")
  }
}

sealed trait Environment {
  val parent: Option[Environment]

  val node: Option[AbstractSyntaxNode]

  def lookup(name: EnvironmentLookup): Option[Referenceable] = {
    // Recursively look up to each parent.
//    println("Looking up " + name + " in \n\t" + this)
    search(name) match {
      case Some(ref) => Some(ref)
      case None => parent.flatMap { env: Environment => env.lookup(name) }
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

  def getEnclosingClassNode: Option[ClassDeclaration] = None

  def getEnclosingCompilationUnit: Option[AbstractSyntaxNode] = None

  /**
   * Get the package scope from the parent. Only the root should respond to this.
   */
  def packageScope(name: PackageNameLookup): Option[Seq[ScopeEnvironment]] = parent match {
    case Some(p: Environment) => p.packageScope(name)
    case _ => None
  }

  def compilationScope: Option[Environment] = parent match {
    case Some(root: RootEnvironment) => Some(this)
    case Some(env: Environment) => env.compilationScope
    case _ => None
  }
}

/**
 * Top level environment, encompassing multiple files' scopes.
 */
class RootEnvironment(nodes: Seq[AbstractSyntaxNode.CompilationUnit]) extends Environment {
  val parent: Option[Environment] = None
  
  val node: Option[AbstractSyntaxNode] = None

  val qualifiedNameMap: Map[QualifiedName, Referenceable] = {
    nodes.foldLeft(Map.empty[QualifiedName, Referenceable]) {
      case (map: Map[QualifiedName, Referenceable], cu: AbstractSyntaxNode.CompilationUnit) => {
        val declaration: TypeDeclaration = cu.typeDeclaration
        val mapping = cu.packageDeclaration match {
          case Some(p: PackageDeclaration) =>
            (TypeName(declaration.name.value, Some(p.name)).toQualifiedName, declaration)
          case _ =>
            (TypeName(declaration.name.value, Some(PackageName(InputString("")))).toQualifiedName, declaration)
        }

        mapping match {
          case (q: QualifiedName, t: TypeDeclaration) => {
            val fqn: TypeName = q.toTypeName
            fqn.scope = t.scope
            t.fullyQualifiedName = Some(fqn)
            map.get(q) match {
              case None => map + (q -> t)
              case Some(_) => throw new SyntaxError("Duplicate qualified name " + q)
            }
          }
        }
      }
    }
  }
  var packageScopeMap: Map[PackageName, Seq[ScopeEnvironment]] = Map.empty[PackageName, Seq[ScopeEnvironment]]

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    name match {
      case TypeNameLookup(qn: QualifiedName) =>
        qualifiedNameMap.get(qn)

      //  The root environment can only handle qualified name lookups.
      case _ => None
    }
  }

  def resolveFullyQualifiedNames() = {
    nodes.foreach {
      case (cu: AbstractSyntaxNode.CompilationUnit) => {
        val declaration: TypeDeclaration = cu.typeDeclaration
        val mapping = cu.packageDeclaration match {
          case Some(p: PackageDeclaration) => (TypeName(declaration.name.value, Some(p.name)).toQualifiedName, declaration)
          case None => (TypeName(declaration.name.value, Some(PackageName(InputString("")))).toQualifiedName, declaration)
        }

        mapping match {
          case (q: QualifiedName, t: TypeDeclaration) => {
            val fqn: TypeName = q.toTypeName
            fqn.scope = t.scope
            t.fullyQualifiedName = Some(fqn)
            if (t.fullyQualifiedName.get.scope == None) {
              throw new SyntaxError("Could not get fully qualified name's scope for " + t)
            }
          }
        }
      }
    }
  }

  override def packageScope(name: PackageNameLookup): Option[Seq[ScopeEnvironment]] = 
    packageScopeMap.get(name.name)
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

  //  Class' sub-scopes can have a class node associated with them for convenience.
  val enclosingClassNode: Option[AbstractSyntaxNode] = None,
  val linkedScopeReferences: Seq[EnvironmentLookup] = Seq.empty[EnvironmentLookup]
) extends Environment {
  val node: Option[AbstractSyntaxNode] = enclosingClassNode

  val parent: Option[Environment] = Some(par)
  override def toString(): String = super.toString()+"<par: @"+Integer.toHexString(par.hashCode())+">[package: " + packageScopeReference + ", imports: " + importScopeReferences + ", links: " + linkedScopes + "](" + locals.keys.toString() + ")"

  override def searchForMethodsWithName(name: InputString): Seq[MethodDeclaration] = {
    var searchScopes = locals.collect{
      case (MethodLookup(qn: QualifiedName, _), method: MethodDeclaration)
      if (qn.toMethodName.value == name) => method
    }.toSeq
    if (EnvironmentLookup.enableLinkedScopes) {
      searchScopes = searchScopes ++ linkedScopes.flatMap(_.searchForMethodsWithName(name))
    }
    searchScopes
  }

  override def getEnclosingClassNode: Option[ClassDeclaration] = node match {
    case Some(a: ClassDeclaration) => Some(a)
    case _ => parent match {
      case None => None
      case Some(s) => s.getEnclosingClassNode
    }
  }

  override def getEnclosingCompilationUnit: Option[AbstractSyntaxNode] = node match {
    case Some(a: CompilationUnit) => Some(a)
    case _ => parent match {
      case None => None
      case Some(s) => s.getEnclosingCompilationUnit
    }
  }

  //  This is not very scala-y or functional, but we need this to link class
  //  scopes to the scopes of their superclasses and interfaces.
  var linkedScopes: Seq[ScopeEnvironment] = Seq.empty[ScopeEnvironment]
  def linkScopesWithMapping(mapping: Map[AbstractSyntaxNode, Environment]): Unit = {
    if (linkedScopes.size == 0 && linkedScopeReferences.size > 0) {
      linkedScopes =
        linkedScopeReferences
        .flatMap(lookup)
        .flatMap(mapping.get(_))
        .collect{case c: ScopeEnvironment => c}
    }
    parent match {
      case Some(s: ScopeEnvironment) => s.linkScopesWithMapping(mapping)
      case _ => Unit
    }
  }

  def fullyQualifyType(t: Type): Type = {
    val result = t match {
      case v: VoidKeyword => v
      case p: PrimitiveType => p
      case c: ClassType => {
        lookup(TypeNameLookup(c.name.toQualifiedName)) match {
          case Some(c: ClassDeclaration) => c.fullyQualifiedName match {
            case Some(fqn: TypeName) => ClassType(fqn)
            case None => throw new SyntaxError("Class " + c.name.niceName + " is missing its fully qualified name.")
          }
          case _ => throw new SyntaxError("Could not fully-qualify class type: " + c.name.niceName)
        }
      }
      case c: InterfaceType => {
        lookup(TypeNameLookup(c.name.toQualifiedName)) match {
          case Some(c: InterfaceDeclaration) => c.fullyQualifiedName match {
            case Some(fqn: TypeName) => InterfaceType(fqn)
            case None => throw new SyntaxError("Interface " + c.name.niceName + " is missing its fully qualified name.")
          }
          case _ => throw new SyntaxError("Could not fully-qualify interface type: " + c.name.niceName)
        }
      }
      case c: ClassOrInterfaceType => {
        lookup(TypeNameLookup(c.name.toQualifiedName)) match {
          case Some(c: ClassDeclaration) => c.fullyQualifiedName match {
            case Some(fqn: TypeName) => ClassType(fqn)
            case None => throw new SyntaxError("Class " + c.name.niceName + " is missing its fully qualified name.")
          }
          case Some(c: InterfaceDeclaration) => c.fullyQualifiedName match {
            case Some(fqn: TypeName) => InterfaceType(fqn)
            case None => throw new SyntaxError("Interface " + c.name.niceName + " is missing its fully qualified name.")
          }
          case _ => throw new SyntaxError("Could not fully-qualify class or interface type: " + c.name.niceName)
        }
      }
      case a: ArrayType => ArrayType(fullyQualifyType(a.subtype))
    }
    result.scope = t.scope
    result
  }

  // Stop searching when our grandparent's getEnclosingClassNode returns None,
  // as we don't want to return the ClassType or InterfaceType.
  def searchInClassOrInterfaceScope(name: EnvironmentLookup): Option[Referenceable] = {
    search(name) match {
      case Some(ref) => Some(ref)
      case None => parent match {
        case Some(p: ScopeEnvironment) => p.parent match {
          case Some(gp: ScopeEnvironment) => gp.getEnclosingClassNode match {
            case Some(_) => p.searchInClassOrInterfaceScope(name)
            case None => None
          }
          case _ => None
        }
        case _ => None
      }
    }
  }

  def search(name: EnvironmentLookup): Option[Referenceable] = {
    locals.get(name) match {
      case Some(r: Referenceable) =>
        Some(r)

      //  Check our other scope references - our package scopes, then our wildcard imports.
      //  Note that wildcard imports are pretty much just additional package scopes, except
      //  for the fact that package scopes are higher priority than wildcard imports.
      case None => {

        //  TODO
        //  For speed (if you want) store the key in the map again.
        name match {
          case ml: MethodLookup =>
            val fullyQualifiedTypes: Seq[Type] = ml.params.map(fullyQualifyType)
            val result = locals.collectFirst {
              case (MethodLookup(name: QualifiedName, params: Seq[Type]), r: Referenceable)
                if ml.name == name && params.map(fullyQualifyType) == fullyQualifiedTypes => r
            }
            result match {
              case None => Unit
              case Some(r: Referenceable) => {
                return Some(r)
              }
            }
          case cl: ConstructorLookup =>
            val fullyQualifiedTypes: Seq[Type] = cl.params.map(fullyQualifyType)
            val result = locals.collectFirst {
              case (ConstructorLookup(name: QualifiedName, params: Seq[Type]), r: Referenceable)
                if cl.name == name && params.map(fullyQualifyType) == fullyQualifiedTypes => r
            }
            result match {
              case None => Unit
              case Some(r: Referenceable) => {
                return Some(r)
              }
            }
          case _ => {}
        }

        val packageResolved: Option[Referenceable]
          = packageScopeReference.toSeq.flatMap(packageScope(_)).flatten.flatMap(env => {
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
              = importScopeReferences.flatMap(packageScope(_)).flatten

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
                if (EnvironmentLookup.enableLinkedScopes) {
                  linkedScopes
                    .filter(_ != this)
                    .toStream
                    .flatMap(_.searchInClassOrInterfaceScope(name))
                    .headOption match {
                      case Some(r: Referenceable) => {
                        Some(r)
                      }
                      case None => None
                  }
                } else {
                  None
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
      (packageScopeReference.toSeq ++ importScopeReferences).flatMap(packageScope(_)).flatten.toSet[ScopeEnvironment].toSeq

    val uniqueDeclarations = uniqueScopes.flatMap(s => {
      s.locals.keys.flatMap(s.lookup(_))
    })

    //val duplicateTypes = types.groupBy(identity).filter {case (_, l) => l.size > 1 }.keys
    //println(duplicateTypes)
  }
}
