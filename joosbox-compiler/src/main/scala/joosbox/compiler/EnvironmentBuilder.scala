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

    new EnvironmentMapping(parent, mapping)
  }

  def traverse(node: AbstractSyntaxNode, parent: Environment, root: RootEnvironment): Map[AbstractSyntaxNode, Environment] = {
    node match {
      //  Special case for Blocks - we need to unroll their contents and make nested scopes.
      case AbstractSyntaxNode.Block(seq: Seq[AbstractSyntaxNode.BlockStatement]) => {
        val e = environmentFromNode(node, parent)

        scopeTreeFromBlockStatements(seq, parent) ++ Map(node -> e) ++ node.children.flatMap(traverse(_, e, root))
      }

      case node: AbstractSyntaxNode => {
        val e = environmentFromNode(node, parent)
        Map(node -> e) ++ node.children.flatMap(traverse(_, e, root))
      }
    }
  }

  def scopeTreeFromBlockStatements(
    statements: Seq[AbstractSyntaxNode.BlockStatement],
    parent: Environment
  ): Map[AbstractSyntaxNode, Environment] = {
    statements match {
      case Seq() => Map.empty[AbstractSyntaxNode, Environment]
      case _ => {
        val (preDecls, decls) = statements.span { 
          case x: AbstractSyntaxNode.LocalVariableDeclaration => false 
          case _ => true
        }

        decls.headOption match {
          case Some(decl: AbstractSyntaxNode.LocalVariableDeclaration) => {
            //  Make a scope that includes all of the pre-declaration statements,
            //  plus the declaration.
            val env = new ScopeEnvironment(Map(NameLookup(decl.name) -> decl), Seq.empty, parent)

            (
              preDecls.map(s => s -> env).toMap ++ Map(decl -> env)
              ++ scopeTreeFromBlockStatements(decls.drop(1), env)
            )
          }

          //  If there is no assignment, group all of the following
          //  statements into the same scope.
          case _ => statements.map(s => s -> parent).toMap
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

          Seq(packageName) ++ imports
        }

        new ScopeEnvironment(locals, otherScopeReferences, parent)
      }

      case n: AbstractSyntaxNode.ClassBody => {
        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], md: AbstractSyntaxNode.MethodDeclaration) => {
              map + (MethodLookup(md.name, md.parameters.map(_.varType)) -> md)
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
        val mapping: Map[EnvironmentLookup, Referenceable] = n.declarations.map(imd => (NameLookup(imd.name), imd)).toMap
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case n: AbstractSyntaxNode.MethodDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (NameLookup(fp.name), fp)).toMap
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case n: AbstractSyntaxNode.Block => {
        val mapping: Map[EnvironmentLookup, Referenceable]
          = n.statements.foldLeft(Map.empty[EnvironmentLookup, Referenceable]) {
            case (map: Map[EnvironmentLookup, Referenceable], l: AbstractSyntaxNode.LocalVariableDeclaration) => {
              val key = IdentifierLookup(l.name)
              map.get(key) match {
                case None => {
                  parent.lookup(key) match {
                    case Some(local: AbstractSyntaxNode.LocalVariableDeclaration)
                      => throw new SyntaxError("Redefinition of " + l.name + " (previous definition: " + local + ")")
                    case _ => map + (key -> l)
                  }
                }
                case Some(r: Referenceable) => throw new SyntaxError("Redefined local variable " + l.name + " (previous definition: " + r + ")")
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], l: AbstractSyntaxNode) => map
          }
        
        new ScopeEnvironment(mapping, Seq.empty, parent)
      }

      case _ => parent
    }
  }
}
