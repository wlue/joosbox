package joosbox.compiler

import joosbox.lexer.{
  SyntaxError,
  InputString
}

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  SingleTypeImportDeclaration,
  TypeImportOnDemandDeclaration,
  PackageDeclaration,
  CompilationUnit,
  Referenceable,
  FormalParameter,
  SimpleName,
  QualifiedName,
  Name
}

object EnvironmentBuilder {
  def build(nodes: Seq[AbstractSyntaxNode.CompilationUnit]): EnvironmentMapping = {
    val parent = new RootEnvironment(nodes)
    val astMapping:Seq[Map[AbstractSyntaxNode, Environment]] = nodes.map(traverse(_, parent, parent))

    val mapping:Map[AbstractSyntaxNode, Environment] = astMapping.reduce(_ ++ _)

    //  Set the parent's packageScopeMap to refer to the new scopes we have.
    parent.packageScopeMap = mapping.foldLeft(Map.empty[QualifiedName, Seq[ScopeEnvironment]]) {
      case (map: Map[QualifiedName, Seq[ScopeEnvironment]], (asn: AbstractSyntaxNode.CompilationUnit, env: Environment)) => {
        env match {
          case scope: ScopeEnvironment => {
            asn.packageDeclaration match {
              case Some(PackageDeclaration(qn: QualifiedName)) => {
                map + (qn -> (map.getOrElse(qn, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }

              case Some(PackageDeclaration(sn: SimpleName)) => {
                val qn = QualifiedName(Seq(sn.value))
                map + (qn -> (map.getOrElse(qn, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }

              case None => {
                val implicitPackage = QualifiedName(Seq(InputString("")))
                map + (implicitPackage -> (map.getOrElse(implicitPackage, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }
            }
          }
          case _ => map
        }
      }
      case (map: Map[QualifiedName, Seq[ScopeEnvironment]], (asn: AbstractSyntaxNode, env: Environment)) => map
    }

    mapping.values.toSet[Environment].collect({ case s: ScopeEnvironment => s }).foreach(s => {
      s.ensureUnambiguousReferences(mapping)
      s.otherScopeReferences.foreach((q: QualifiedNameLookup) => {
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
      case AbstractSyntaxNode.Block(seq: Seq[AbstractSyntaxNode.BlockStatement]) => {

        val e = new ScopeEnvironment(Map.empty, Seq.empty, parent)
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
    statements: Seq[AbstractSyntaxNode.BlockStatement],
    parent: Environment,
    root: RootEnvironment
  ): Map[AbstractSyntaxNode, Environment] = {
    statements match {
      case Seq() => Map.empty[AbstractSyntaxNode, Environment]
      case _ => {
        val (preDecls, decls) = statements.span { 
          case x: AbstractSyntaxNode.LocalVariableDeclaration => false
          case AbstractSyntaxNode.ForStatement(Some(v: AbstractSyntaxNode.ForVariableDeclaration), _, _, _) => false
          case _ => true
        }

        decls.headOption match {
          case Some(decl: AbstractSyntaxNode.LocalVariableDeclaration) => {
            //  Declaration implicitly creates a new scope for itself and everything after it.
            val key = IdentifierLookup(decl.name)
            parent.lookup(key) match {
              case Some(local: AbstractSyntaxNode.LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + local + ")")
              case Some(forv: AbstractSyntaxNode.ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + forv + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), Seq.empty, parent)
                (
                  preDecls.map(s => s -> parent).toMap ++ Map(decl -> env)
                  ++ scopeTreeFromBlockStatements(decls.drop(1), env, root)
                )
              }
            }
          }

          case Some(AbstractSyntaxNode.ForStatement(
            Some(decl: AbstractSyntaxNode.ForVariableDeclaration),
            check: Option[AbstractSyntaxNode.Expression],
            update: Option[AbstractSyntaxNode.StatementExpression],
            statement: AbstractSyntaxNode.Statement
          )) => {
            val key = IdentifierLookup(decl.variableName)
            parent.lookup(key) match {
              case Some(local: AbstractSyntaxNode.LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + local + ")")
              case Some(forv: AbstractSyntaxNode.ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + forv + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), Seq.empty, parent)
                (
                  Map(decl -> env) ++
                  (check.toSeq ++ update.toSeq ++ Seq(statement)).flatMap(traverse(_, env, root)).toMap
                )
              }
            }

            statements.flatMap(traverse(_, parent, root)).toMap
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
      case n: AbstractSyntaxNode.CompilationUnit => {
        //  Locals of a CompilationUnit should contain all of its
        //  defined classes and interfaces, as well as all of its
        //  explicit imports.

        //  otherScopes of a CompilationUnit should include:
        //      All other scopes in its package
        //    followed by
        //      All * imports.

        //  All single type imports should be fully qualified.
        val explicitImports: Map[NameLookup, Referenceable] = n.importDeclarations.flatMap {
          case i: SingleTypeImportDeclaration => {
            i.name match {
              case q: QualifiedName => {
                parent.search(QualifiedNameLookup(q)) match {
                  case Some(r: Referenceable) => Some(NameLookup(q.value.last) -> r)
                  case None => throw new SyntaxError("Type import '" + i.name + "' not found.")
                }
              }

              case s: SimpleName => {
                parent.search(QualifiedNameLookup(QualifiedName(Seq(s.value)))) match {
                  case Some(r: Referenceable) => Some(NameLookup(s.value) -> r)
                  case None => throw new SyntaxError("Type import '" + i.name + "' not found.")
                }
              }
            }
          }
          case _ => None
        }.toMap

        val locals: Map[EnvironmentLookup, Referenceable] = (
          n.typeDeclaration.map(x => (NameLookup(x.name), x)).toMap
          ++ explicitImports
        )

        val otherScopeReferences: Seq[QualifiedNameLookup] = {
          val packageName:QualifiedNameLookup = n.packageDeclaration match {
            case Some(PackageDeclaration(qn: QualifiedName)) => QualifiedNameLookup(qn)
            case Some(PackageDeclaration(sn: SimpleName)) => QualifiedNameLookup(QualifiedName(Seq(sn.value)))

            //  The "default package", the empty string.
            case None => QualifiedNameLookup(QualifiedName(Seq(InputString(""))))
          }

          val imports:Seq[QualifiedNameLookup] = n.importDeclarations.flatMap {
            case TypeImportOnDemandDeclaration(qn: QualifiedName) => Some(QualifiedNameLookup(qn))
            case TypeImportOnDemandDeclaration(sn: SimpleName) => Some(QualifiedNameLookup(QualifiedName(Seq(sn.value))))
            case _ => None
          }

          Seq(packageName, QualifiedNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"))))) ++ imports
        }

        new ScopeEnvironment(locals, otherScopeReferences, parent)
      }

      case n: AbstractSyntaxNode.ClassBody => {
        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], cd: AbstractSyntaxNode.ConstructorDeclaration) => {
              val key = ConstructorLookup(cd.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate constructor declaration for " + cd.name)
                case None => map + (key -> cd)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], md: AbstractSyntaxNode.MethodDeclaration) => {
              val key = MethodLookup(md.name, md.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate method declaration for " + md.name)
                case None => map + (key -> md)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], fd: AbstractSyntaxNode.FieldDeclaration) => {
              val key = IdentifierLookup(fd.name)
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate field declaration for " + fd.name)
                case None => map + (key -> fd)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], asn: AbstractSyntaxNode) => map
          })
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case n: AbstractSyntaxNode.InterfaceBody => {
        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], md: AbstractSyntaxNode.InterfaceMemberDeclaration) => {
              val key = MethodLookup(md.name, md.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate method declaration for " + md.name)
                case None => map + (key -> md)
              }
            }
          })
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case n: AbstractSyntaxNode.MethodDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (NameLookup(fp.name), fp)).toMap
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case _ => parent
    }
  }
}
