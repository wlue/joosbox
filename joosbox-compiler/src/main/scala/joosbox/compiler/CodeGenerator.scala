package joosbox.compiler

import joosbox.parser.{AbstractSyntaxNode, ScopeEnvironment, MethodLookup}
import joosbox.parser.AbstractSyntaxNode._
import joosbox.lexer.{SyntaxError, InputString}

/**
 * Created by psobot on 3/22/14.
 */
object CodeGenerator {

  lazy val preamble: String =
    scala.io.Source.fromFile("joosbox-compiler/src/test/resources/stdlib/defines.s").mkString

  /*
    Returns a map of class names to strings, for storage in files by the caller.
   */
  def generateAssembly(units: Seq[CompilationUnit]): Map[String, String] = {
    val first: Map[String, String] = units.headOption match {
      case Some(cu: CompilationUnit) => {
        val method: MethodDeclaration = cu.typeDeclaration.scope match {
          case Some(scope: ScopeEnvironment) => {
            val lookup = MethodLookup(MethodName(InputString("test")).toQualifiedName, Seq.empty[Type])
            scope.lookup(lookup) match {
              case Some(md : MethodDeclaration) => md
              case _ => throw new SyntaxError("Could not find method declaration matching signature `int test()`.")
            }
          }
          case _ => throw new SyntaxError("Could not find scope on type declaration: " + cu.typeDeclaration)
        }

        val entryMethodSymbol = method.symbolName
        val asm = preamble + s"""
extern initFields
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
      Map("initFields" -> (preamble + generateInitFields(units))) ++
      units.map(cu => cu.assemblyFileName -> (preamble + generateAssemblyForNode(cu)))
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
      val instanceOfEntries = cd.instanceOfList.map(x => s"InstanceOfEntry($x)").mkString("\n")
      val methodsForVtable = cd.methodsForVtable

      val requiredImports = methodsForVtable.filter{_.scope.get.getEnclosingClassNode.get != cd}.map(
        x => s"extern ${x.symbolName}"
      ).mkString("\n")

      val requiredClassTags = methodsForVtable
        .filter{_.scope.get.getEnclosingClassNode.get != cd}
        .map(_.scope.get.getEnclosingClassNode.get.asInstanceOf[ClassDeclaration])
        .toSet[ClassDeclaration]
        .map(x => s"%define ${x.symbolName}_class_tag 0x${x.runtimeTag.toHexString}")
        .mkString("\n")

      val methodsForTopLevel = methodsForVtable.map(
        x => s"VTableMethodDef($symbolName, ${x.symbolName}, ${x.symbolName})"
      ).mkString("\n")

      s"""
$requiredImports
$requiredClassTags

SECTION .data

%define ${symbolName}_class_tag 0x$runtimeTag

; instanceof array for $symbolName
InstanceOfHeader($symbolName)
$instanceOfEntries
InstanceOfEnd
; end of instanceof array for $symbolName

; beginning of vtable for $symbolName
VTableClassHeader($symbolName)
VTableInstanceOfRef($symbolName)
$methodsForTopLevel
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


  def pushArguments(node:AbstractSyntaxNode) : Seq[String] = {
    var args : Seq[Expression] = Seq.empty
    node match {
      case smi: SimpleMethodInvocation => args = smi.args
      case cmi: ComplexMethodInvocation => args = cmi.args
      case ccp: ClassCreationPrimary => args = ccp.args
      case _ => Unit
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
