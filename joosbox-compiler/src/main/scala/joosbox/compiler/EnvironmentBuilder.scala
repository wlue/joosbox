package joosbox.compiler

import joosbox.lexer.{
  SyntaxError,
  InputString
}

import joosbox.parser.AbstractSyntaxNode
import joosbox.parser.AbstractSyntaxNode._

object EnvironmentBuilder {
  def build(nodes: Seq[CompilationUnit]): EnvironmentMapping = {
    val parent = new RootEnvironment(nodes)
    val astMapping:Seq[Map[AbstractSyntaxNode, Environment]] = nodes.map(traverse(_, parent, parent))

    val mapping:Map[AbstractSyntaxNode, Environment] = astMapping.reduce(_ ++ _)

    //  Set the parent's packageScopeMap to refer to the new scopes we have.
    parent.packageScopeMap = mapping.foldLeft(Map.empty[PackageName, Seq[ScopeEnvironment]]) {
      case (map: Map[PackageName, Seq[ScopeEnvironment]], (asn: CompilationUnit, env: Environment)) => {
        env match {
          case scope: ScopeEnvironment => {
            asn.packageDeclaration match {
              case Some(PackageDeclaration(pn: PackageName)) => {
                val prefixMap: Map[PackageName, Seq[ScopeEnvironment]] =
                  pn.toQualifiedName.prefixes.map { prefix =>
                    (prefix.toPackageName, map.getOrElse(prefix.toPackageName, Seq.empty[ScopeEnvironment]))
                  }.toMap

                map ++ prefixMap + (pn -> (map.getOrElse(pn, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }

              case None => {
                val implicitPackage = PackageName(InputString(""))
                map + (implicitPackage -> (map.getOrElse(implicitPackage, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }
            }
          }
          case _ => map
        }
      }
      case (map: Map[PackageName, Seq[ScopeEnvironment]], (asn: AbstractSyntaxNode, env: Environment)) => map
    }

    mapping.values.toSet[Environment].collect({ case s: ScopeEnvironment => s }).foreach(s => {
      s.ensureUnambiguousReferences(mapping)
      s.linkScopesWithMapping(mapping)
      s.importScopeReferences.foreach((q: PackageNameLookup) => {
        parent.packageScopeMap.get(q.name) match {
          case None => throw new SyntaxError("Attempted on-demand import " + q.name + " could not be found.")
          case Some(_) => Unit
        }
      })
    })

    new EnvironmentMapping(parent, mapping)
  }

  def traverse(node: AbstractSyntaxNode, parent: Environment, root: RootEnvironment): Map[AbstractSyntaxNode, Environment] = {
    node match {
      //  Special case for Blocks - we need to unroll their contents and make nested scopes.
      case Block(seq: Seq[BlockStatement]) => {

        val e = new ScopeEnvironment(Map.empty, None, Seq.empty, parent)
        val scopetree = scopeTreeFromBlockStatements(seq, e, root)
        node.children.flatMap(traverse(_, e, root)).toMap ++ scopetree ++ Map(node -> e)
      }

      case node: AbstractSyntaxNode => {
        val e = environmentFromNode(node, parent)
        Map(node -> e) ++ node.children.flatMap(traverse(_, e, root))
      }
    }
  }

  def scopeTreeFromBlockStatements(
    statements: Seq[BlockStatement],
    parent: Environment,
    root: RootEnvironment
  ): Map[AbstractSyntaxNode, Environment] = {
    statements match {
      case Seq() => Map.empty[AbstractSyntaxNode, Environment]
      case _ => {
        val (preDecls, decls) = statements.span { 
          case x: LocalVariableDeclaration => false
          case ForStatement(Some(v: ForVariableDeclaration), _, _, _) => false
          case _ => true
        }

        decls.headOption match {
          case Some(decl: LocalVariableDeclaration) => {
            //  Declaration implicitly creates a new scope for itself and everything after it.
            val key = ExpressionNameLookup(decl.name)
            parent.lookup(key) match {
              case Some(local: LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + local + ")")
              case Some(forv: ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + forv + ")")
              case Some(param: FormalParameter)
                => throw new SyntaxError("Redefinition of " + decl.name + " (conflicts with parameter: " + param + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), None, Seq.empty, parent)
                (
                  preDecls.flatMap(traverse(_, parent, root)).toMap
                  ++ Map(decl -> env)
                  ++ scopeTreeFromBlockStatements(decls.drop(1), env, root)
                )
              }
            }
          }

          case Some(ForStatement(
            Some(decl: ForVariableDeclaration),
            check: Option[Expression],
            update: Option[StatementExpression],
            statement: Statement
          )) => {
            val key = ExpressionNameLookup(decl.variableName)
            parent.lookup(key) match {
              case Some(local: LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + local + ")")
              case Some(forv: ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + forv + ")")
              case Some(param: FormalParameter)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (conflicts with parameter: " + param + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), None, Seq.empty, parent)
                (
                  Map(decl -> env) ++
                  (check.toSeq ++ update.toSeq ++ Seq(statement)).flatMap(traverse(_, env, root)).toMap
                  ++ scopeTreeFromBlockStatements(decls.drop(1), parent, root)
                )
              }
            }
          }

          //  If there is no assignment, group all of the following
          //  statements into the same scope and continue traversing.
          case _ => statements.flatMap(s => traverse(s, parent, root)).toMap
        }
      }
    }
  }

  def environmentFromNode(node: AbstractSyntaxNode, parent: Environment): Environment = {
    node match {
      case n: CompilationUnit => {
        //  Locals of a CompilationUnit should contain all of its
        //  defined classes and interfaces, as well as all of its
        //  explicit imports.

        //  otherScopes of a CompilationUnit should include:
        //      All other scopes in its package
        //    followed by
        //      All * imports.

        //  All single type imports should be fully qualified.
        val explicitImports: Map[TypeNameLookup, Referenceable] = n.importDeclarations.flatMap {
          case i: SingleTypeImportDeclaration => {
            i.name match {
              case t: TypeName => {
                parent.search(TypeNameLookup(t)) match {
                  case Some(r: Referenceable) => Some(TypeNameLookup(TypeName(t.value)) -> r)
                  case None => throw new SyntaxError("Type import '" + i.name + "' not found in " + parent)
                }
              }
            }
          }
          case _ => None
        }.toMap

        val locals: Map[EnvironmentLookup, Referenceable] = (
          n.typeDeclaration.map(x => (TypeNameLookup(x.name), x)).toMap
          ++ explicitImports
        )

        val packageScopeReference: PackageNameLookup = {
          n.packageDeclaration match {
            case Some(PackageDeclaration(pn: PackageName)) => PackageNameLookup(pn)

            //  The "default package", the empty string.
            case None => PackageNameLookup(PackageName(InputString("")))
          }
        }

        val importScopeReferences: Seq[PackageNameLookup] = {
          val imports:Seq[PackageNameLookup] = n.importDeclarations.flatMap {
            case TypeImportOnDemandDeclaration(pn: PackageName) => Some(PackageNameLookup(pn))
            case SingleTypeImportDeclaration(tn: TypeName) => None
          }

          Seq(PackageNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"))).toPackageName)) ++ imports
        }

        new ScopeEnvironment(locals, Some(packageScopeReference), importScopeReferences, parent)
      }

      case cd: ClassDeclaration => {
        val n: ClassBody = cd.body

        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], cd: ConstructorDeclaration) => {

              //  TODO: We shouldn't just catch conflicting parameter /names/,
              //  these should technically be different scopes for each param...
              if (cd.parameters.map(_.name).toSet.size != cd.parameters.size) {
                throw new SyntaxError("Duplicate parameter names in constructor.")
              }

              val key = ConstructorLookup(cd.name.value, cd.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate constructor declaration for " + cd.name)
                case None => map + (key -> cd)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], md: MethodDeclaration) => {

              //  TODO: We shouldn't just catch conflicting parameter /names/,
              //  these should technically be different scopes for each param...
              if (md.parameters.map(_.name).toSet.size != md.parameters.size) {
                throw new SyntaxError("Duplicate parameter names in constructor.")
              }

              val key = MethodLookup(md.name.value, md.parameters.map(_.varType))

              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate method declaration for " + md.name)
                case None => map + (key -> md)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], fd: FieldDeclaration) => {
              val key = ExpressionNameLookup(fd.name)
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate field declaration for " + fd.name)
                case None => map + (key -> fd)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], asn: AbstractSyntaxNode) => map
          })

        //  Include all superclasses and implemented interfaces in this scope.
        val linkedScopeReferences: Seq[EnvironmentLookup] = {
          /*
          (cd.superclass.toSeq ++ cd.interfaces).flatMap {
            case ClassType(tn: TypeName) => Some(TypeNameLookup(tn))
            case InterfaceType(tn: TypeName) => Some(TypeNameLookup(tn))
            case _ => None
          }*/

          //  For now, just leave this empty because it might break our hierarchy checker.
          Seq.empty[EnvironmentLookup]
        }

        new ScopeEnvironment(mapping, None, Seq.empty, parent, linkedScopeReferences)
      }

      case n: InterfaceBody => {
        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], md: InterfaceMemberDeclaration) => {
              val key = MethodLookup(md.name, md.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate method declaration for " + md.name)
                case None => map + (key -> md)
              }
            }
          })
        new ScopeEnvironment(mapping, None, Seq.empty, parent)
      }

      case n: MethodDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (ExpressionNameLookup(fp.name), fp)).toMap
        new ScopeEnvironment(mapping, None, Seq.empty, parent)
      }

      case n: ConstructorDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (ExpressionNameLookup(fp.name), fp)).toMap
        new ScopeEnvironment(mapping, None, Seq.empty, parent)
      }

      case _ => parent
    }
  }
}
