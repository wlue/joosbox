package joosbox.compiler

import joosbox.parser.{ScopeEnvironment, Parser, AbstractSyntaxNode, EnvironmentBuilder}
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode.CompilationUnit

object CompilerRunner {
  def main(args: Array[String]) {
    if (args.size >= 1) {
      try {
        runTestable(args)
      } catch {
        case se: SyntaxError =>
          System.err.println(se)
          System.exit(42)
        case matche: MatchError =>
          System.err.println("Got unexpected MatchError (probably improperly implemented syntax): " + matche)
          System.exit(42)
        case noel: java.util.NoSuchElementException =>
          System.err.println("Got unexpected NoSuchElementException (probably improperly implemented syntax): " + noel)
          System.exit(42)
        case fnfe: java.io.FileNotFoundException =>
          System.err.println(fnfe)
          System.exit(42)
      }
    } else {
      println("Usage: joosc <file.joos> <file2.joos> ...")
      System.exit(42)
    }
  }

  def runTestable(args: Array[String]) {
    val nodes: Seq[CompilationUnit] = args.map { filename: String =>
      Parser.Joos.parseFilename(filename)
    }

    val root = EnvironmentBuilder.build(nodes)
    TypeLinker.link(nodes, root)
    HierarchyChecker.link(nodes)

    // nodes.foreach(enableScopeLinking(_))

    // NameLinker.link(nodes)
    // TypeChecker.link(nodes)
  }

  def enableScopeLinking(node: AbstractSyntaxNode): Unit = {
    node.scope match {
      case Some(scope: ScopeEnvironment) => scope.useLinkedScopes = true
      case _ => Unit
    }
    node.children.foreach(enableScopeLinking(_))
  }
}
