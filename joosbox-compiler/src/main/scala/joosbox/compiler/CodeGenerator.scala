package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode._

/**
 * Created by psobot on 3/22/14.
 */
object CodeGenerator {

  /*
    Returns a map of class names to strings, for storage in files by the caller.
   */
  def generateAssembly(units: Seq[CompilationUnit]): Map[String, String] = {
    val first = units.headOption match {
      case Some(x: CompilationUnit) =>
        Map(x.assemblyFileName -> generateAssemblyForEntryPoint(x))
      case None => Map.empty[String, String]
    }
    first ++ Map.empty[String, String]
  }

  /*
    One of the generated .s files must define the global symbol _start:

      global _start
      _start:

    When your program is run, execution will start from this point.
    Unlike in Java, the first method that begins executing is not
    static void main(String[]), but static int test(). All of the
    test inputs will have such a method. The class containing the
    test method will be listed first on the joosc command line,
    before any other compilation units. The code that you generate
    at _start should initialize all static fields, then call this method.
    When the method returns with return value x, your program should exit
    with exit code x using the sys_exit system call.
    To execute this system call, load the value 1 (indicating sys_exit)
    into register eax, load the exit code into register ebx, then execute
    the instruction int 0x80.
  */
  def generateAssemblyForEntryPoint(cu: CompilationUnit): String = {
    //  TODO: Implement me properly. This currently just returns 1.
    """
global _start
_start:
  mov eax, 1
  mov ebx, 1
  int 0x80

    """.stripMargin
  }
}
