package joosbox.compiler

import joosbox.parser.{
  AbstractSyntaxNode
}

import AbstractSyntaxNode.{
  CompilationUnit,
  Referenceable,
  FormalParameter,
  SimpleName,
  Name
}

object EnvironmentBuilder {
  def build(nodes: Seq[AbstractSyntaxNode.CompilationUnit]): EnvironmentMapping = {
    val parent = new CompilationEnvironment()
    val astMapping:Seq[Map[AbstractSyntaxNode, Environment]] = nodes.map(traverse(_, parent))    
    new EnvironmentMapping(parent, astMapping.reduce(_ ++ _))
  }

  def traverse(node: AbstractSyntaxNode, parent: Environment): Map[AbstractSyntaxNode, Environment] = {
    val environment: Option[Environment] = environmentFromNode(node, parent)
    environment match {
      case Some(e: Environment) => Map(node -> e) ++ node.children.flatMap(traverse(_, e))
      case None => node.children.flatMap(traverse(_, parent)).toMap
    }
  }

  def environmentFromNode(node: AbstractSyntaxNode, parent: Environment): Option[Environment] = {
    node match {
      case n: AbstractSyntaxNode.CompilationUnit => {
        val mapping: Map[EnvironmentLookup, Referenceable] = (
          n.interfaceDeclarations.map(x => (NameLookup(x.name), x)).toMap
          ++ n.classDeclaration.map(x => (NameLookup(x.name), x)).toMap
        )
        Some(new ScopeEnvironment(parent, mapping))
      }

      case n: AbstractSyntaxNode.ClassBody => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.declarations.flatMap({
          case md: AbstractSyntaxNode.MethodDeclaration =>
            Some((MethodLookup(md.name, md.parameters.map(_.varType)), md))
          case fd: AbstractSyntaxNode.FieldDeclaration => Some((NameLookup(fd.name), fd))
          case _ => None
        }).toMap
        Some(new ScopeEnvironment(parent, mapping))
      }

      case n: AbstractSyntaxNode.InterfaceBody => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.declarations.map(imd => (NameLookup(imd.name), imd)).toMap
        Some(new ScopeEnvironment(parent, mapping))
      }

      case n: AbstractSyntaxNode.MethodDeclaration => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.parameters.map(fp => (NameLookup(fp.name), fp)).toMap
        Some(new ScopeEnvironment(parent, mapping))
      }

      case n: AbstractSyntaxNode.Block => {
        val mapping: Map[EnvironmentLookup, Referenceable] = n.statements.flatMap({
          case l: AbstractSyntaxNode.LocalVariableDeclaration => Some(NameLookup(l.name), l)
          case _ => None
        }).toMap
        
        Some(new ScopeEnvironment(parent, mapping))
      }

      case _ => None
    }
  }
}
