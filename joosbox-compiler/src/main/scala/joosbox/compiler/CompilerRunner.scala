package joosbox.compiler

import joosbox.parser.Parser
import joosbox.parser.SyntaxError

object CompilerRunner {
  def main(args: Array[String]) {
    if (args.size == 1) {
      try {
        Parser.Joos.parseFilename(args.head)
      } catch {
        case se: SyntaxError => {
          println(se)
          System.exit(42)
        }
      }
    } else {
      println("Usage: joosc <file.joos>")
      System.exit(42)
    }
  }
}
