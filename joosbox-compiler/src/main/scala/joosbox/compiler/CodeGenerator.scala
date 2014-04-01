package joosbox.compiler

import joosbox.parser.{AbstractSyntaxNode, ScopeEnvironment, EnvironmentLookup, MethodLookup, TypeNameLookup, ConstructorLookup}
import joosbox.parser.AbstractSyntaxNode._
import joosbox.lexer.{SyntaxError, InputString}

/**
 * Created by psobot on 3/22/14.
 */
object CodeGenerator {

  lazy val preamble: String =
    scala.io.Source.fromFile("joosbox-compiler/src/test/resources/stdlib/defines.s").mkString

  def generateSingleAssembly(units: Seq[CompilationUnit]): String = {
    preamble + generateAssembly(units).map({
      case (f: String, d: String) => {
        s"""

; ====================
; BEGINNING OF FILE $f
$d
; END OF FILE $f
; ====================

        """
      }
    }).mkString("\n")
  }

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
        val asm = s"""
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
      units.map(cu => cu.assemblyFileName -> generateAssemblyForNode(cu)(Some(cu), None))
    )
  }

  def generateInitFields(units: Seq[CompilationUnit]): String = {
    s"""
global initFields
initFields:
  ret

    """.stripMargin
  }

  def generateAssemblyForNode(
    n: AbstractSyntaxNode,
    indent: Integer = 0
  )(
    implicit parentCompilationUnit: Option[CompilationUnit],
    parentClassDeclaration: Option[ClassDeclaration]
  ): String = {
    val asm = n match {
      case cd: ClassDeclaration => {
        //  Generate vtable for this class
        val symbolName = cd.symbolName
        val runtimeTag = cd.runtimeTag.toHexString
        val instanceOfEntries = cd.instanceOfList.map(x => s"InstanceOfEntry($x)").mkString("\n")
        val methodsForVtable = cd.methodsForVtable

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
          case Some(b: Block) => {

            //  Find all of the local variables declared within this method, and
            //  make room for them at the start of the stack frame.
            def findLocalVariableDeclarations(b: AbstractSyntaxNode): Seq[StackAllocatedVariable] = b match {
              case l: LocalVariableDeclaration => Seq(l)
              case f: ForVariableDeclaration => Seq(f)
              case x => x.children.flatMap(findLocalVariableDeclarations)
            }

            val locals: Seq[String] = findLocalVariableDeclarations(b).map(_.symbolName)
            val localAccessDefinitions = locals.zipWithIndex.map{case (id, i) => s"%define $id [ebp - ${4 * (i + 1)}]"}.mkString("\n")

            s"sub esp, ${locals.size * 4}\n" + localAccessDefinitions + "\n" + generateAssemblyForNode(b, indent + 1)
          }
          case _ => ""
        }

        s"""
$symbolName:
    push ebp
    mov ebp, esp   ; save the stack pointer
$body
    mov esp, ebp   ; reset the stack pointer
    pop ebp
    ret; end of method $symbolName
"""
      }

      case cd: ConstructorDeclaration => {
        val symbolName = cd.symbolName

        val body: String = cd.body match {
          case Some(b: Block) => {

            //  Find all of the local variables declared within this method, and
            //  make room for them at the start of the stack frame.
            def findLocalVariableDeclarations(b: AbstractSyntaxNode): Seq[StackAllocatedVariable] = b match {
              case l: LocalVariableDeclaration => Seq(l)
              case f: ForVariableDeclaration => Seq(f)
              case x => x.children.flatMap(findLocalVariableDeclarations)
            }

            val locals: Seq[String] = findLocalVariableDeclarations(b).map(_.symbolName)
            val localAccessDefinitions = locals.zipWithIndex.map{case (id, i) => s"%define $id [ebp - ${4 * i}]"}.mkString("\n")

            s"sub esp, ${locals.size * 4}\n" + localAccessDefinitions + "\n" + generateAssemblyForNode(b, indent + 1)
          }
          case _ => ""
        }

        s"""
$symbolName:
    push ebp
    mov ebp, esp   ; save the stack pointer
$body
    mov esp, ebp   ; reset the stack pointer
    pop ebp
    ret; end of method $symbolName
"""
      }

      case m: SimpleMethodInvocation => {
        val env = m.scope.get
        val an = m.name
        TypeChecker.resolveMethodName(an, m.args, env) match {
          case Some(declaration: MethodDeclaration) => {
            //  TODO: Need to lookup what the prefix of this method declaration is.
            //  If it's not static, then load the expressionName that is its prefix into the

            val symbolName: String = declaration.symbolName
            val classSymbolName: String = declaration.scope.get.getEnclosingClassNode.get.symbolName
            val call: String = if (declaration.isStatic) {
              s"call $symbolName\n"
            } else {
              println("Calling simpleMethodInvocation on expression (?) " + an.prefix)
              val loadInvokeTargetIntoEAX = an.prefix match {
                case None => "mov eax, 0xcafebabe; TODO: load the 'this' pointer of this method"
                case Some(an: AmbiguousName) => {
                  NameLinker.disambiguateName(an)(env) match {
                    case e: ExpressionName => generateAssemblyForNode(e, indent + 1)
                  }
                }
              }

              s"""
  $loadInvokeTargetIntoEAX
  mov eax, [eax + ObjectVTableOffset]
  VMethodCall(eax, $classSymbolName, $symbolName)
"""
            }

            val pushedArgs = pushArguments(m)
            pushedArgs.mkString("\n") + call + s"""
add esp, ${pushedArgs.size * 4} ; remove the "this" and params from the stack
            """
          }
          case None =>
            throw new SyntaxError("Could not resolve method: " + an)
          case _ =>
            throw new SyntaxError("Method resolved to an abstract method declaration: " + an)
        }

      }

      case m: ComplexMethodInvocation => {
        val env = m.scope.get
        val an = m.name
        TypeChecker.resolveMethodName(an, m.args, env) match {
          case Some(declaration: MethodDeclaration) => {
            val symbolName: String = declaration.symbolName
            val classSymbolName: String = declaration.scope.get.getEnclosingClassNode.get.symbolName
            val call: String = s"""
  mov eax, [eax + ObjectVTableOffset]
  VMethodCall(eax, $classSymbolName, $symbolName)
"""
            //  Generate the assembly for the primary (pushing it onto the stack)
            //  then pop the primary's result into ebx and call it
            generateAssemblyForNode(m.primary, indent + 1) + pushArguments(m).mkString("\n") + call
          }
          case None =>
            throw new SyntaxError("Could not resolve method: " + an)
          case _ =>
            throw new SyntaxError("Method resolved to an abstract method declaration: " + an)
        }

      }

      case b: Block => {
        b.statements
         .map(generateAssemblyForNode(_, indent + 1))
         .filter{_ != ""}
         .mkString("\n")
      }

      case r: ReturnStatement => {
        val expr = r.expression match {
          case Some(e: Expression) => generateAssemblyForNode(e, indent + 1)
          case None => ""
        }
        expr +  """
    mov esp, ebp   ; reset the stack pointer
    pop ebp
    ret
"""
      }

      case l: LocalVariableDeclaration => {
        val exprAsm = generateAssemblyForNode(l.expression, indent + 1)
        s"""
$exprAsm
mov ${l.symbolName}, eax
"""
      }

      //  This is a read of an expressionname
      case e: ExpressionName => {
        e.scope.get.lookup(EnvironmentLookup.lookupFromName(e)) match {
          case Some(l: LocalVariableDeclaration) => s"mov eax, ${l.symbolName}\n"
          case Some(f: ForVariableDeclaration) => s"mov eax, ${f.symbolName}\n"
          case Some(f: FieldDeclaration) => s"mov eax, 0xcafecafe; TODO: field declaration lookup\n"
          case Some(f: FormalParameter) => s"mov eax, 0xdeadcafe; TODO: formal parameter lookup\n"

          //  TODO: Handle the "None" case, which happens if we call array.length or if we can't find a lookup.
          case _ => ""

          case x =>
            throw new SyntaxError("Environment lookup for name " + e.niceName + " resulted in unknown node " + x)
        }
      }

      case a: Assignment => {
        a.leftHandSide match {
          case f: FieldAccess => ""

          //  This is a write to an expressionname
          case e: ExpressionName => e.scope.get.lookup(EnvironmentLookup.lookupFromName(e)) match {
            case Some(l: LocalVariableDeclaration) => s"mov ${l.symbolName}, eax\n"
            case Some(f: ForVariableDeclaration) => s"mov ${f.symbolName}, eax\n"
            case Some(f: FieldDeclaration) => s"mov eax, 0; TODO: field declaration assignment\n"
            case Some(f: FormalParameter) => s"mov eax, 0; TODO: formal parameter assignment\n"

            //  TODO: Handle the "None" case, which happens if we call array.length or if we can't find a lookup.
            case _ => ""

            case x =>
              throw new SyntaxError("Environment lookup for name " + e.niceName + " resulted in unknown node " + x)
          }

          case s: SimpleArrayAccess => ""

          case c: ComplexArrayAccess => ""

          case x
            => throw new SyntaxError("Cannot assign to " + x)
        }
      }

      case n: Num => s"mov eax, ${n.value}\n"

      case _: FalseLiteral => s"mov eax, 0\n"
      case _: TrueLiteral => s"mov eax, 1\n"

      case AddExpression(e1, e2) => (
        generateAssemblyForNode(e1, indent + 1) + "push eax\n"
        + generateAssemblyForNode(e2, indent + 1) + "push eax\n"
        + """
pop eax
pop ebx
add eax, ebx
        """
      )
      case SubtractExpression(e1, e2) => (
        generateAssemblyForNode(e1, indent + 1) + "push eax\n"
        + generateAssemblyForNode(e2, indent + 1) + "push eax\n"
        + """
pop ebx
pop eax
sub eax, ebx
        """
      )
      case MultiplyExpression(e1, e2) => (
        generateAssemblyForNode(e1, indent + 1) + "push eax\n"
        + generateAssemblyForNode(e2, indent + 1) + "push eax\n"
        + """
pop eax
pop edx
imul edx
        """
      )
      case DivideExpression(e1, e2) => (
        generateAssemblyForNode(e1, indent + 1) + "push eax\n"
        + generateAssemblyForNode(e2, indent + 1) + "push eax\n"
        + """
pop ebx
pop eax
cdq
idiv ebx
        """
      )

      case c : ClassCreationPrimary => {
        val env = c.scope.get

        def recursiveFields(decl : ClassDeclaration): Set[String] = {
          val fields:Set[String] = decl.body.declarations.filter({
              case f:FieldDeclaration if !f.isStatic => true
              case _ => false
          }).map( f => f.asInstanceOf[FieldDeclaration].name.value.value).toSet

          if (!decl.superclass.isEmpty) {
            fields ++ env.lookup(TypeNameLookup(decl.superclass.get.name.toQualifiedName)) match {
              case sdecl: ClassDeclaration => recursiveFields(sdecl)
              case _ => Set.empty[String]
            }
          } else {
            fields
          }
        }

        val classDecl:ClassDeclaration = env.lookup(TypeNameLookup(c.classType.name.toQualifiedName)) match {
          case Some(cdecl: ClassDeclaration) => cdecl
          case _ => throw new SyntaxError("Invalid class creation.")
        }
        val constructorTypes: Seq[Type] = TypeChecker.resolvedTypesForArgs(c.args, env)
        val classLookup: EnvironmentLookup = TypeNameLookup(c.classType.fullyQualifiedName)
        val constructorDecl: ConstructorDeclaration = env.lookup(classLookup) match {
          case Some(cd: ClassDeclaration) => {
            val constructorLookup: EnvironmentLookup = ConstructorLookup(c.classType.name.toQualifiedName, constructorTypes)
            cd.scope.get.lookup(constructorLookup) match {
              case Some(cdecl: ConstructorDeclaration) => cdecl
              case _
                => throw new SyntaxError("Could not find constructor: " + c.classType.name.niceName)
            }
          }
          case _
            => throw new SyntaxError("Could not find class declaration for " + classLookup)
        }

        val fields = recursiveFields(classDecl)
        val allocSize = (fields.size + 1)
        val vtableBase = s"VTableBase(${classDecl.symbolName})"
        val pushedArgs = pushArguments(c)
        val classSymbol = classDecl.symbolName
        val constructorSymbol = constructorDecl.symbolName

        s"""
        mov eax, ${allocSize * 4}
        call __malloc
        push eax ; push the new object onto the stack
        mov dword [eax], $vtableBase

        ${pushedArgs.mkString("\n")}
        mov ebx, [eax + ObjectVTableOffset]
        VMethodCall(ebx, $classSymbol, $constructorSymbol)

        add esp, ${pushedArgs.size * 4} ; remove the "this" and params from the stack

        ; the top of the stack now contains the "this" pointer
        pop eax
        """
      }

      case x => x.children.map(generateAssemblyForNode(_, indent + 1)).filter{_ != ""}.mkString("\n")
    }

    s"""
; begin asm for node ${n.getClass.getSimpleName} 0x${n.hashCode.toHexString}
$asm
; end asm for node ${n.getClass.getSimpleName} 0x${n.hashCode.toHexString}
    """
  }


  def pushArguments(node:AbstractSyntaxNode, isStatic: Boolean = false) : Seq[String] = {
    val args : Seq[Expression] = node match {
      case smi: SimpleMethodInvocation => smi.args
      case cmi: ComplexMethodInvocation => cmi.args
      case ccp: ClassCreationPrimary => ccp.args
      case x => throw new SyntaxError("Cannot push arguments for node type without arguments: " + x)
    }

    // Go right-to-left for method parameters
    if (isStatic) {
      args.reverse.map({ a => pushToStackSlot(allocateStackSlot(a.slot)) })
    } else {
      args.reverse.map({ a => pushToStackSlot(allocateStackSlot(a.slot)) }) ++ Seq("push eax; 'this' pointer\n")
    }
  }

  def allocateStackSlot(offset:Integer) : String = s"dword [ebp - ${offset * 4}]"
  def pushToStackSlot(location:String) : String = s"push 0 ;;$location\n"
}
