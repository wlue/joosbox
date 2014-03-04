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

              case None => map
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
    val environment: Option[Environment] = environmentFromNode(node, parent)
    environment match {
      case Some(e: Environment) => Map(node -> e) ++ node.children.flatMap(traverse(_, e, root))
      case None => node.children.flatMap(traverse(_, parent, root)).toMap
    }
  }

  def environmentFromNode(node: AbstractSyntaxNode, parent: Environment): Option[Environment] = {
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

        Some(new ScopeEnvironment(locals, otherScopeReferences, parent))
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
        Some(new ScopeEnvironment(mapping, Seq.empty, parent))
      }

      case n: AbstractSyntaxNode.InterfaceBody => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.declarations.map(imd => (NameLookup(imd.name), imd)).toMap
        Some(new ScopeEnvironment(mapping, Seq.empty, parent))
      }

      case n: AbstractSyntaxNode.MethodDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (NameLookup(fp.name), fp)).toMap
        Some(new ScopeEnvironment(mapping, Seq.empty, parent))
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
        
        Some(new ScopeEnvironment(mapping, Seq.empty, parent))
      }

      case _ => None
    }
  }
}
