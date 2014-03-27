package joosbox.compiler

import joosbox.parser.{AbstractSyntaxNode, ScopeEnvironment, MethodLookup}
import joosbox.parser.AbstractSyntaxNode._
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
      units.map(cu => cu.assemblyFileName -> generateAssemblyForNode(cu))
    ).toMap
  }

  def generateInitFields(units: Seq[CompilationUnit]): String = {
    s"""
global initFields
initFields:
  ret

    """.stripMargin
  }

  def generateAssemblyForNode(n: AbstractSyntaxNode, indent: Integer = 0): String = n match {
    case cd: ClassDeclaration => {
      //  Generate vtable for this class
      val symbolName = cd.symbolName
      val runtimeTag = cd.runtimeTag.toHexString
      s"""
SECTION .data
; beginning of vtable for $symbolName
vtable_$symbolName:
  dd 0x$runtimeTag  ; class tag for $symbolName


; end of vtable for $symbolName
SECTION .text
""" + cd.children.map(generateAssemblyForNode(_)).filter{_ != ""}.mkString("\n")
    }

    case md: MethodDeclaration => {
      val symbolName = md.symbolName
      val body: String = md.body match {
        case Some(b: Block) => generateAssemblyForNode(b, indent + 1)
        case None => ("  " * indent) + "ret\n"
      }

      (
        ("  " * indent) + s"global $symbolName\n" +
        ("  " * indent) + s"$symbolName:\n" + body
      )
    }
    case b: Block => {
      b.statements.map(generateAssemblyForNode(_, indent + 1)).filter{_ != ""}.mkString("\n")
    }

    case r: ReturnStatement => {
      //  TODO: This is just for testing.
      r.expression match {
        case Some(n: Num) => {
          val numval = n.value
          ("  " * indent) + s"mov eax, $numval\n" + ("  " * indent) + "ret\n"
        }

        case None => ("  " * indent) + "ret\n"

        //  TODO: handle this case, this is just super late night testing.
        case _ => ("  " * indent) + "ret\n"
      }
    }
    case x => x.children.map(generateAssemblyForNode(_, indent + 1)).filter{_ != ""}.mkString("\n")
  }


  def pushMethodArguments(m:MethodInvocation) : Seq[String] = {
    var args : Seq[Expression] = Seq.empty
    m match {
      case s: SimpleMethodInvocation =>
        args = s.args
      case c: ComplexMethodInvocation =>
        args = c.args
    }

    // Go right-to-left for method parameters
    // TODO: Implicit this parameter
    args.reverse.map({ a => pushToStackSlot(allocateStackSlot(a.slot)) })
  }

  def allocateStackSlot(offset:Integer) : String = {
    s"dword [ebp - " + (offset * 4) + "]"
  }

  def pushToStackSlot(location:String) : String = {
    s"push " + location
  }
}
