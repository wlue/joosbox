package joosbox.compiler

import joosbox.lexer.SyntaxError

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  SingleTypeImportDeclaration,
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
    new EnvironmentMapping(parent, astMapping.reduce(_ ++ _))
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
          n.interfaceDeclarations.map(x => (NameLookup(x.name), x)).toMap
          ++ n.classDeclaration.map(x => (NameLookup(x.name), x)).toMap
          ++ explicitImports
        )
        Some(new ScopeEnvironment(locals, Seq.empty, parent))
      }

      case n: AbstractSyntaxNode.ClassBody => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.declarations.flatMap({
          case md: AbstractSyntaxNode.MethodDeclaration =>
            Some((MethodLookup(md.name, md.parameters.map(_.varType)), md))
          case fd: AbstractSyntaxNode.FieldDeclaration => Some((IdentifierLookup(fd.name), fd))
          case _ => None
        }).toMap
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
        val mapping: Map[EnvironmentLookup, Referenceable] = n.statements.flatMap({
          case l: AbstractSyntaxNode.LocalVariableDeclaration => Some(NameLookup(l.name), l)
          case _ => None
        }).toMap
        
        Some(new ScopeEnvironment(mapping, Seq.empty, parent))
      }

      case _ => None
    }
  }
}
