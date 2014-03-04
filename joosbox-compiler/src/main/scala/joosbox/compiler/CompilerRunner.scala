package joosbox.compiler

import joosbox.parser.Parser
import joosbox.lexer.SyntaxError
import joosbox.parser.AbstractSyntaxNode

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

    val mapping = EnvironmentBuilder.build(nodes)
    TypeLinker.link(nodes, mapping)
    HierarchyChecker.link(nodes, mapping)
  }
}
