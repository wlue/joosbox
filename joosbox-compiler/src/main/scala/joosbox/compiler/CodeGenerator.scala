package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode._
import joosbox.parser.{ScopeEnvironment, MethodLookup}
import joosbox.lexer.{SyntaxError, InputString}

/**
 * Created by psobot on 3/22/14.
 */
object CodeGenerator {

  /*
    Returns a map of class names to strings, for storage in files by the caller.
   */
  def generateAssembly(units: Seq[CompilationUnit]): Map[String, String] = {
    val first: Map[String, String] = units.headOption match {
      case Some(cu: CompilationUnit) => {
        val method: MethodDeclaration = cu.typeDeclaration match {
          case Some(t: TypeDeclaration) => t.scope match {
            case Some(scope: ScopeEnvironment) => {
              val lookup = MethodLookup(MethodName(InputString("test")).toQualifiedName, Seq.empty[Type])
              scope.lookup(lookup) match {
                case Some(md : MethodDeclaration) => md
                case _ => throw new SyntaxError("Could not find method declaration matching signature `int test()`.")
              }
            }
            case _ => throw new SyntaxError("Could not find scope on type declaration: " + t)
          }
          case _ => throw new SyntaxError("Could not find type declaration in first compilation unit provided: " + cu)
        }

        val entryMethodSymbol = method.symbolName
        val asm = s"""
extern initFields
extern __debexit
extern $entryMethodSymbol

global _start
_start:
  call initFields
  call $entryMethodSymbol
  call __debexit

    """.stripMargin
        Map("bootstrap" -> asm)
      }
      case None => throw new SyntaxError(
        "No files passed in on the command line!"
      )
    }
    (
      first ++
      Map("initFields" -> generateInitFields(units)) ++
      units.map(cu => cu.assemblyFileName -> generateAssemblyForClass(cu))
    ).toMap
  }

  def generateInitFields(units: Seq[CompilationUnit]): String = {
    s"""
global initFields
initFields:
  ret

    """.stripMargin
  }

  def generateAssemblyForClass(cu: CompilationUnit): String = {
    val methods: Seq[MethodDeclaration] = cu.typeDeclaration match {
      case Some(cd: ClassDeclaration) => {
        cd.body.declarations.collect { case c: MethodDeclaration => c }
      }
      case _ => Seq.empty[MethodDeclaration]
    }

    methods.map(m => {
      val symbolName = m.symbolName
      s"""
global $symbolName
$symbolName:
  mov eax, 99
  ret
      """.stripMargin
    }).mkString("\n")
  }
}
