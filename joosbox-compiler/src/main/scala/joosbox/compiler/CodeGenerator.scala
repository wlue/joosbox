package joosbox.compiler

import joosbox.parser.{AbstractSyntaxNode, Environment, ScopeEnvironment, EnvironmentLookup, MethodLookup, TypeNameLookup, ConstructorLookup}
import joosbox.parser.AbstractSyntaxNode._
import joosbox.lexer.{SyntaxError, InputString}

/**
 * Created by psobot on 3/22/14.
 */

object NASMDefines {
  //  The version of NASM used by Marmoset does not support label concatenation properly.
  //  We'll do it in our own compiler, instead.

  def VTableBase(s: String): String = s"vtable_${s}"
  def VMethodLabel(klass: String, method: String): String = s"vtable_${klass}_method__${method}"
  def VMethodCall(reg: String, klass: String, method: String): String =
    s"call [${reg} + (vtable_${klass}_method__${method} - vtable_${klass})]"

  def ClassTagForClass(klass: String): String = s"${klass}_class_tag"

  def VTableClassHeader(klass: String): String =
    s"vtable_${klass}:"
  def VTableNestedClassHeader(klass: String, superklass: String): String =
    s"vtable_${klass}_${superklass}:"

  def VTableStaticFieldTag(klass: String, field: String): String =
    s"vtable_${klass}_static_field_${field}"

  def VTableStaticFieldDef(klass: String, field: String): String =
    s"${VTableStaticFieldTag(klass, field)}: dd 0x0"

  def VTableMethodDef(klass: String, method: String, impl: String): String =
    s"vtable_${klass}_method__${method}: dd ${impl}"
  def VTableNestedMethodDef(klass: String, superklass: String, method: String, impl: String): String =
    s"vtable_${klass}_method__${method}_${superklass}: dd ${impl}"

  def GetVTableOffset(klass: String): String = OffsetTableLabel(klass)
  def OffsetTableLabel(klass: String): String = s"offsettable_${klass}"
  def OffsetTableStatementLabel(klass: String, midclass: String): String = s"offsettable_${klass}_${midclass}"
  def OffsetTableStatementLabelEnd(klass: String, midclass: String): String = s"offsettable_${klass}_${midclass}_nomatch"

  def GetArrayVTableOffset(klass: String): String = ArrayOffsetTableLabel(klass)
  def ArrayOffsetTableLabel(klass: String): String = s"offsettable_array_${klass}"
  def ArrayOffsetTableLabelReferenceType: String = s"offsettable_array_reftype"
  def ArrayOffsetTableStatementLabel(klass: String, midclass: String): String = s"offsettable_array_${klass}_${midclass}"
  def ArrayOffsetTableStatementLabelEnd(klass: String, midclass: String): String = s"offsettable_array_${klass}_${midclass}_nomatch"

  def InstanceOfHeader(klass: String): String = s"instanceof_${klass}:"
  def InstanceOfEntry(otherclass: String): String = s"dd ${otherclass}_class_tag"
  def InstanceOfEnd: String = "dd 0x0"

  def VTableOffsetFunction(klass: ClassDeclaration): String = s"""

  """
}

object CodeGenerator {

  val arraySymbolName : String = "java_lang_Array"

  lazy val preamble: String =
    scala.io.Source.fromFile("joosbox-compiler/src/test/resources/stdlib/defines.s").mkString

  def generateSingleAssembly(units: Seq[CompilationUnit]): String = {
    preamble + generateJavaLangArrayVTable(units(0).scope.get) + generateAssembly(units).map({
      case ((f: String, d: String)) => {
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
  def generateAssembly(units: Seq[CompilationUnit]): Seq[(String, String)] = {
    val first: String = units.headOption match {
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
        asm
      }
      case None => throw new SyntaxError(
        "No files passed in on the command line!"
      )
    }
    Seq(
      ("bootstrap", first),
      ("offsetTables", generateOffsetTables(units)),
      ("initFields", generateInitFields(units)),
      ("stringLiterals", generateStringLiteralsData(units))
    ) ++ units.map(cu => cu.assemblyFileName -> generateAssemblyForNode(cu)(Some(cu), None))
  }

  def generateOffsetTables(units: Seq[CompilationUnit]): String = {
    val ancestors: Set[TypeDeclaration] = Set(
      units(0).scope.get.lookup(TypeNameLookup(CommonNames.JavaIOSerializable)).get.asInstanceOf[TypeDeclaration],
      units(0).scope.get.lookup(TypeNameLookup(CommonNames.JavaLangCloneable)).get.asInstanceOf[TypeDeclaration],
      units(0).scope.get.lookup(TypeNameLookup(CommonNames.JavaLangObject)).get.asInstanceOf[TypeDeclaration]
    )

    var subclassesIncludingSelfs: Map[String, Set[String]]
      = Map.empty[String, Set[String]]
    var classTags: Seq[String] = Seq.empty[String]

    def addMapping(superc: String, subc: String) = {
      val s: Set[String]
        = subclassesIncludingSelfs.getOrElse(superc, Set.empty[String])
      subclassesIncludingSelfs
        = subclassesIncludingSelfs ++ Map(superc -> (s ++ Set(subc)))
    }

    def gatherClassRelationships(n: AbstractSyntaxNode): Unit = {
      n match {
        case c: ClassDeclaration => {
          classTags = classTags ++ Seq(s"%define ${c.symbolName}_class_tag 0x${c.runtimeTag.toHexString}")

          if (!c.modifiers.contains(AbstractKeyword())) {
            addMapping(c.symbolName, c.symbolName)
            ancestors.foreach(x => addMapping(x.symbolName, c.symbolName))
            if (c.superclass.isDefined) {
              val superclass:ClassDeclaration =
                c.scope.get.lookup(TypeNameLookup(c.superclass.get.name.toQualifiedName)) match {
                  case Some(cdecl: ClassDeclaration) => cdecl
                  case _ => throw new SyntaxError("Invalid class creation.")
                }

              addMapping(superclass.symbolName, c.symbolName)
            }
            c.interfaces.foreach{case i: InterfaceType => {
              c.scope.get.lookup(TypeNameLookup(i.name.toQualifiedName)) match {
                case Some(idecl: InterfaceDeclaration) => addMapping(idecl.symbolName, c.symbolName)
                case _ => throw new SyntaxError("Invalid interface creation.")
              }
            }}
          }
        }
        case x => x.children.map(gatherClassRelationships)
      }
    }

    units.map(gatherClassRelationships)

    val offsetTable = subclassesIncludingSelfs.map{
      case (s: String, sub: Set[String]) => {
        var substatements = sub.map(x => {
          val movExpr = if (x == s) {
            "0"
          } else {
            s"(vtable_${x}_${s} - vtable_${x})"
          }

          s"""
${NASMDefines.OffsetTableStatementLabel(s, x)}:
  cmp eax, ${NASMDefines.ClassTagForClass(x)}
  jne ${NASMDefines.OffsetTableStatementLabelEnd(s, x)}
  mov eax, $movExpr
  ret
${NASMDefines.OffsetTableStatementLabelEnd(s, x)}:
"""
        })

        if (s == ancestors.last.symbolName) { // if javaLangObject
          substatements = substatements ++ Seq(s"""
${NASMDefines.OffsetTableStatementLabel(s, arraySymbolName)}:
  cmp eax, ${NASMDefines.ClassTagForClass(arraySymbolName)}
  jne ${NASMDefines.OffsetTableStatementLabelEnd(s, arraySymbolName)}
  mov eax, (vtable_${arraySymbolName}_$s - vtable_$arraySymbolName)
  ret
${NASMDefines.OffsetTableStatementLabelEnd(s, arraySymbolName)}:
            """)
        }

        s"""
; beginning of offset table for $s
${NASMDefines.OffsetTableLabel(s)}:
; move class tag into eax
mov eax, [eax + ObjectClassTagOffset]
${substatements.mkString("\n")}

; lookup failed - return a constant
mov eax, NoVTableOffsetFound
ret
; end of offset table for $s

; beginning of offset table for array of $s
${NASMDefines.ArrayOffsetTableLabel(s)}:
; assume that eax contains the pointer to the array object
; check if the provided object is a class
mov ebx, [eax + ObjectClassTagOffset]
cmp ebx, ${NASMDefines.ClassTagForClass(arraySymbolName)}
jne .isNotArray

; we are casting an array, so we should check that the subtypes are equal
; pretend like our array-contained classtag is at the correct offset,
; so that when the sub-method dereferences the classtag on the "object",
; it gets back the classtag of the contained objects within the array
; this is like a very dirty thunk
add eax, ArrayTypeOffset
call ${NASMDefines.GetVTableOffset(s)}

; If this is a valid cast, then we get a possibly nonzero offset
; so we need to map the return code of this call like so:
;  if eax == NOMATCH => return NOMATCH
;  else return an offset of 0
cmp eax, NoVTableOffsetFound
je .isInvalidCast

; else return an offset of 0 - arrays cannot be overridden, their vtables are always just Object
mov eax, 0
ret

.isInvalidCast:
.isNotArray:
; lookup failed - return a constant
mov eax, NoVTableOffsetFound
ret
; end of offset table for array of $s
"""
      }
    }.mkString("\n\n")

    val arrayReferenceTypeMapping = s"""
; beginning of offset table for array of reference types
${NASMDefines.ArrayOffsetTableLabelReferenceType}:
; assume that eax contains the pointer to the array object
; check if the provided object is a class
mov ebx, [eax + ObjectClassTagOffset]
cmp ebx, ${NASMDefines.ClassTagForClass(arraySymbolName)}
jne .isNotArray

; we are casting an array of reference types, so the subtypes will be equal
; return an offset of 0 - arrays cannot be overridden, their vtables are always just Object
mov eax, 0
ret

.isNotArray:
; lookup failed - return a constant
mov eax, NoVTableOffsetFound
ret
; end of offset table for array of reference types
"""

    val arrayHexString:String = arraySymbolName.hashCode.toHexString
    val arrayTag:String = s"%define ${arraySymbolName}_class_tag 0x$arrayHexString"
    (classTags ++ Seq(arrayTag)).mkString("\n") + offsetTable + arrayReferenceTypeMapping
  }

  def generateOffsetCallForType(c: Type): String = c match {
    case ct: ClassOrInterfaceType => {
      c.scope.get.lookup(TypeNameLookup(ct.name.toQualifiedName)) match {
        case Some(cd: ClassDeclaration) => s"call ${NASMDefines.OffsetTableLabel(cd.symbolName)}"
        case Some(id: InterfaceDeclaration) => s"call ${NASMDefines.OffsetTableLabel(id.symbolName)}"
        case x => throw new SyntaxError("Cast expression could not find target class or interface, instead got: " + x)
      }
    }
    case ct: ClassType => {
      c.scope.get.lookup(TypeNameLookup(ct.name.toQualifiedName)) match {
        case Some(cd: ClassDeclaration) => s"call ${NASMDefines.OffsetTableLabel(cd.symbolName)}"
        case _ => throw new SyntaxError("Cast expression could not find target class")
      }
    }
    case it: InterfaceType => {
      c.scope.get.lookup(TypeNameLookup(it.name.toQualifiedName)) match {
        case Some(id: InterfaceDeclaration) => s"call ${NASMDefines.OffsetTableLabel(id.symbolName)}"
        case _ => throw new SyntaxError("Cast expression could not find target interface")
      }
    }

    case ArrayType(ct: ClassOrInterfaceType) => {
      c.scope.get.lookup(TypeNameLookup(ct.name.toQualifiedName)) match {
        case Some(cd: ClassDeclaration) => s"call ${NASMDefines.ArrayOffsetTableLabel(cd.symbolName)}"
        case Some(id: InterfaceDeclaration) => s"call ${NASMDefines.ArrayOffsetTableLabel(id.symbolName)}"
        case x => throw new SyntaxError("Cast expression could not find target array class or interface, instead got: " + x)
      }
    }

    case ArrayType(ct: ClassType) => {
      c.scope.get.lookup(TypeNameLookup(ct.name.toQualifiedName)) match {
        case Some(cd: ClassDeclaration) => s"call ${NASMDefines.ArrayOffsetTableLabel(cd.symbolName)}"
        case _ => throw new SyntaxError("Cast expression could not find target array class")
      }
    }

    case ArrayType(it: InterfaceType) => {
      c.scope.get.lookup(TypeNameLookup(it.name.toQualifiedName)) match {
        case Some(id: InterfaceDeclaration) => s"call ${NASMDefines.ArrayOffsetTableLabel(id.symbolName)}"
        case _ => throw new SyntaxError("Cast expression could not find target array interface")
      }
    }
    case ArrayType(_: ArrayType) => throw new SyntaxError("Nested arrays not supported")
    case ArrayType(_: VoidKeyword) => throw new SyntaxError("Arrays of void type not supported")
    case ArrayType(_: PrimitiveType) => "nop ; primitive cast"
    case _: PrimitiveType => "nop ; primitive cast"
  }

  def generateInitFields(units: Seq[CompilationUnit]): String = {
    val code: String = units.flatMap { unit => unit match {
      case CompilationUnit(_, _, classDecl: ClassDeclaration) => classDecl match {
        case ClassDeclaration(_, ClassBody(declarations), _, _, _) => {
          val code = declarations.flatMap {
            case field: FieldDeclaration if field.isStatic => field.expression match {
              case Some(expr) => {
Some(s"""
  ; Field: ${field.name.niceName}
  ${generateAssemblyForNode(expr)(Some(unit), Some(classDecl))}
  mov [${NASMDefines.VTableStaticFieldTag(classDecl.symbolName, field.symbolName)}], eax
""".toString)
              }
              case _ => None
            }
            case _ => None
          }

          if (!code.isEmpty) {
            Some(s"; Static assignment for class ${classDecl.name.niceName}:\n" ++ code.mkString("\n"))
          } else {
            None
          }
        }
      }
      case _ => None
    }}.mkString("\n")

    s"""
global initFields
initFields:
${code}
  ret

    """.stripMargin
  }

  var string_literals : Seq[String] = Seq.empty[String]

  def buildStringLiteralIndexData(node: AbstractSyntaxNode):String = {
    var literalAsm:String = ""
    node match {
      case s:StringLiteral => {
        if (!string_literals.contains(s.stringVal)) {
          string_literals = string_literals ++ Seq(s.stringVal)
          s.index = Some(string_literals.indexOf(s.stringVal))
          val stringSymbolName = node.scope.get.lookup(
            TypeNameLookup(CommonNames.JavaLangString)
          ) match {
            case Some(c: ClassDeclaration) => c.symbolName
            case _ => throw new SyntaxError("Could not find javalangstring")
          }

          val charsAsm:String = s.stringVal.toList.map{ char => s"dd '$char'"}.mkString("\n")
          literalAsm = s"""
_string_literal_${s.index.get}_char_array:
dd ${NASMDefines.ClassTagForClass(arraySymbolName)}
dd ${NASMDefines.VTableBase(arraySymbolName)}
dd CharTypeTag
dd ${s.stringVal.length}
$charsAsm

_string_literal_${s.index.get}_String:
dd ${NASMDefines.ClassTagForClass(stringSymbolName)}
dd ${NASMDefines.VTableBase(stringSymbolName)}
dd _string_literal_${s.index.get}_char_array
          """
        } else {
          s.index = Some(string_literals.indexOf(s.stringVal))
        }
      }
      case _ => Unit
    }

    val childAsm:Seq[String] = node.children.map { child =>
      buildStringLiteralIndexData(child)
    }

    (Seq(literalAsm) ++ childAsm).filter(x => x != "").mkString("\n")
  }

  def generateStringLiteralsData(units: Seq[CompilationUnit]): String = {
    string_literals = Seq.empty[String]

    val literalsAsm:String = units.map { unit =>
      buildStringLiteralIndexData(unit)
    }.mkString("\n")

    s"""
SECTION .data
; beginning of string literals data
$literalsAsm
; end of string literals data
SECTION .text
  """.stripMargin
  }

  def generateNestedVTableEntries(td: TypeDeclaration): Seq[TypeDeclaration] = {
    def gatherParentClassHierarchy(n: AbstractSyntaxNode): Seq[TypeDeclaration] = {
      n match {
        case c: ClassDeclaration => {
          val interfaces = c.interfaces.map{case i: InterfaceType => {
            c.scope.get.lookup(TypeNameLookup(i.name.toQualifiedName)) match {
              case Some(idecl: InterfaceDeclaration) => idecl
              case _ => throw new SyntaxError("Invalid interface creation.")
            }
          }}
          if (c.superclass.isDefined) {
            val superclass:ClassDeclaration =
              c.scope.get.lookup(TypeNameLookup(c.superclass.get.name.toQualifiedName)) match {
                case Some(cdecl: ClassDeclaration) => cdecl
                case _ => throw new SyntaxError("Invalid class creation.")
              }
            interfaces ++ Seq(superclass)
          } else {
            interfaces
          }
        }
        case x => x.children.flatMap(gatherParentClassHierarchy)
      }
    }

    gatherParentClassHierarchy(td) ++ Seq(
      td.scope.get.lookup(TypeNameLookup(CommonNames.JavaLangObject)).get.asInstanceOf[TypeDeclaration],
      td.scope.get.lookup(TypeNameLookup(CommonNames.JavaIOSerializable)).get.asInstanceOf[TypeDeclaration],
      td.scope.get.lookup(TypeNameLookup(CommonNames.JavaLangCloneable)).get.asInstanceOf[TypeDeclaration]
    )
  }

  def getInstanceFieldsForClass(cd: ClassDeclaration): Seq[FieldDeclaration] = {
    //  Ordering of instance fields:
    //    super...superclass's fields
    //    ...
    //    superclass's fields
    //    class's fields

    val fields: Seq[FieldDeclaration] = cd.body.declarations.filter({
      case f:FieldDeclaration if !f.isStatic => true
      case _ => false
    }).map(f => f.asInstanceOf[FieldDeclaration])

    if (!cd.superclass.isEmpty) {
      (cd.scope.get.lookup(TypeNameLookup(cd.superclass.get.name.toQualifiedName)) match {
        case Some(sdecl: ClassDeclaration) => getInstanceFieldsForClass(sdecl)
        case _ => Seq.empty[FieldDeclaration]
      }) ++ fields
    } else {
      fields
    }
  }

  def getOffsetOfInstanceFieldInClass(f: FieldDeclaration, cd: ClassDeclaration): Integer =
    getInstanceFieldsForClass(cd).indexOf(f)

  def getOffsetOfInstanceField(f: FieldDeclaration): Integer = {
    if (f.isStatic) {
      throw new SyntaxError("Trying to get offset of static field in instance!")
    }
    getInstanceFieldsForClass(f.scope.get.getEnclosingClassNode.get.asInstanceOf[ClassDeclaration]).indexOf(f)
  }

  def generateJavaLangArrayVTable(env:Environment) : String = {
    val symbolName:String = arraySymbolName
    val objDecl:ClassDeclaration = env.lookup(TypeNameLookup(CommonNames.JavaLangObject)).get.asInstanceOf[ClassDeclaration]

    val nestedEntries:String = (
      Seq(NASMDefines.VTableNestedClassHeader(symbolName, objDecl.symbolName)) ++
      objDecl.methodsForVtable.map( x =>
        NASMDefines.VTableNestedMethodDef(symbolName, objDecl.symbolName, x.symbolName, x.symbolName)
      )
    ).mkString("\n")

      s"""
SECTION .data

; beginning of vtable for $symbolName
${NASMDefines.VTableClassHeader(symbolName)}
$nestedEntries
; end of vtable for $symbolName

; vtable offset lookup functions for $symbolName

; end of vtable offset lookup functions for $symbolName

SECTION .text
  """
  }

  def generateAssemblyForNode(n: AbstractSyntaxNode)(
    implicit parentCompilationUnit: Option[CompilationUnit],
    parentClassDeclaration: Option[ClassDeclaration],
    hasDereferencedPrefix: Boolean = false
  ): String = {
    val asm = n match {
      case cd: ClassDeclaration => {
        //  Generate vtable for this class
        val symbolName = cd.symbolName
        val instanceOfEntries = cd.instanceOfList.map(x => NASMDefines.InstanceOfEntry(x)).mkString("\n")

        val localMethods = cd.methodsForVtable
        val staticFields = cd.staticFieldsForVtable.map { field =>
          NASMDefines.VTableStaticFieldDef(symbolName, field.symbolName)
        }.mkString("\n")
        val methodsForTopLevel = localMethods.map { x =>
          NASMDefines.VTableMethodDef(symbolName, x.symbolName, x.symbolName)
        }.mkString("\n")

        def generateReferenceToOverriddenMethod(superSymbolName: String, n: MethodOrConstructorDeclaration): String = n match {
          case m: MethodDeclaration => {
            cd.scope.get.lookup(MethodLookup(m.name.toQualifiedName, m.parameters.map(_.varType))) match {
              case Some(concrete: MethodDeclaration) =>
                NASMDefines.VTableNestedMethodDef(symbolName, superSymbolName, m.symbolName, concrete.symbolName)
              case None
                => throw new SyntaxError("Could not find concrete method declaration for VTable: " + m.niceName)
            }
          }
          case constructor: ConstructorDeclaration => {
            cd.scope.get.lookup(ConstructorLookup(constructor.name.toQualifiedName, constructor.parameters.map(_.varType))) match {
              case Some(concrete: ConstructorDeclaration) =>
                NASMDefines.VTableNestedMethodDef(symbolName, superSymbolName, constructor.symbolName, concrete.symbolName)
              case None
                => throw new SyntaxError("Could not find concrete constructor declaration for VTable: " + constructor.name.niceName)
            }
          }
        }

        val nestedEntries = generateNestedVTableEntries(cd).flatMap{
          case c: ClassDeclaration => {
            Seq(NASMDefines.VTableNestedClassHeader(symbolName, c.symbolName)) ++
            c.methodsForVtable.map(generateReferenceToOverriddenMethod(c.symbolName, _))
          }
          case i: InterfaceDeclaration => {
            Seq(NASMDefines.VTableNestedClassHeader(symbolName, i.symbolName)) ++
            i.methodsForVtable.map(generateReferenceToOverriddenMethod(i.symbolName, _))
          }
        }.mkString("\n")

        s"""
SECTION .data

; instanceof array for $symbolName
${NASMDefines.InstanceOfHeader(symbolName)}
$instanceOfEntries
${NASMDefines.InstanceOfEnd}
; end of instanceof array for $symbolName

; beginning of vtable for $symbolName
${NASMDefines.VTableClassHeader(symbolName)}
$staticFields
$methodsForTopLevel
$nestedEntries
; end of vtable for $symbolName

; vtable offset lookup functions for $symbolName

; end of vtable offset lookup functions for $symbolName

SECTION .text
  """ + cd.children.map(generateAssemblyForNode).filter{_ != ""}.mkString("\n")
      }

      case md: MethodDeclaration => {
        val symbolName = md.symbolName

        md.body match {
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

            val parameterDefinitions = md.parameters.reverse.zipWithIndex.map{
              case (fp: FormalParameter, i: Int) => s"%define ${fp.symbolName}_${fp.hashCode} [ebp + ${4 * (if (md.isStatic) (i + 2) else (i + 3))}]"
            }.mkString("\n")

            val parameterDefinitionsWithThis = if (md.isStatic) {
              parameterDefinitions
            } else {
              parameterDefinitions + "\n" + s"%define ${md.symbolName}_${md.hashCode}_this [ebp + 8]"
            }

            val body = generateAssemblyForNode(b)
            s"""
$symbolName:
push ebp
mov ebp, esp   ; save the stack pointer
sub esp, ${locals.size * 4}
$localAccessDefinitions
$parameterDefinitionsWithThis
$body
mov esp, ebp   ; reset the stack pointer
pop ebp
ret; end of method $symbolName
"""
          }

          case None if md.modifiers.contains(NativeKeyword()) => {
            val fullMethodName = md.scope.get.getEnclosingClassNode.get.fullyQualifiedName.get.toQualifiedName.value.map(_.value).mkString(".") + "." + md.name.value.value
            s"""
$symbolName:
; native method invocation
extern NATIVE$fullMethodName
call NATIVE$fullMethodName
ret; end of method $symbolName
"""
          }
          case _ => s"""
$symbolName:
; empty method invocation
ret; end of method $symbolName
"""
        }
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

            val parameterDefinitions = cd.parameters.zipWithIndex.map{
              case (fp: FormalParameter, i: Int) => s"%define ${fp.symbolName}_${fp.hashCode} [ebp + ${4 * (i+3)}]"
            }.mkString("\n")

            val parameterDefinitionsWithThis = parameterDefinitions + "\n" + s"%define ${cd.symbolName}_${cd.hashCode}_this [ebp + 8]"

            val body = generateAssemblyForNode(b)
            s"""
sub esp, ${locals.size * 4}
$localAccessDefinitions
$parameterDefinitionsWithThis
$body
            """
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
            val symbolName: String = declaration.symbolName
            val classSymbolName: String = declaration.scope.get.getEnclosingClassNode.get.symbolName
            val call: String = if (declaration.isStatic) {
              s"call $symbolName"
            } else {
              val loadInvokeTargetIntoEAX = an.prefix match {

                //  TODO: If we could easily get the caller here, this should be a call to
                //  mov eax, ${caller.symbolName}_${caller.hashCode}_this
                case None => s"mov eax, [ebp + 8]; load the 'this' pointer"
                case Some(an: AmbiguousName) => {
                  NameLinker.disambiguateName(an)(env) match {
                    case e: ExpressionName => generateAssemblyForNode(e)
                    case _
                      => throw new SyntaxError("Could not disambiguate expression name for invocation target: " + an)
                  }
                }
                case _
                  => throw new SyntaxError("Could not get invocation target: " + an)
              }
              val thunkAsm = s"""
; move vtable of the invocation target into ebx
mov ebx, [eax + ObjectVTableOffset]
; call the appropriate method to move the vtable offset into eax
call ${NASMDefines.GetVTableOffset(classSymbolName)}

cmp eax, NoVTableOffsetFound
je __exception
; add the offset and the vtable pointer we stored in ebx
add eax, ebx
; eax now contains the vtable pointer at the appropriate offset
              """
              generateInstanceCallAssembly(loadInvokeTargetIntoEAX, thunkAsm, classSymbolName, symbolName)
            }

            val pushArgs = pushArguments(m)
            val argsSize = m.args.size
            s"""
$pushArgs
$call
add esp, ${argsSize * 4} ; remove the params from the stack
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
        val argTypes: Seq[Type] = TypeChecker.resolvedTypesForArgs(m.args, env)
        val scope:Environment = TypeChecker.resolvePrimaryAndFindScope(m.primary, env)

        TypeChecker.resolveMethodName(an, m.args, scope) match {
          case Some(declaration: MethodDeclaration) => {
            val symbolName: String = declaration.symbolName
            val classSymbolName: String = declaration.scope.get.getEnclosingClassNode.get.symbolName

            val primaryAsm:String = generateAssemblyForNode(m.primary)
            val thunkAsm = s"""
; move vtable of the invocation target into ebx
mov ebx, [eax + ObjectVTableOffset]
; call the appropriate method to move the vtable offset into eax
call ${NASMDefines.GetVTableOffset(classSymbolName)}
cmp eax, NoVTableOffsetFound
je __exception
; add the offset and the vtable pointer we stored in ebx
add eax, ebx
; eax now contains the vtable pointer at the appropriate offset
              """
            val call:String = generateInstanceCallAssembly(primaryAsm, thunkAsm, classSymbolName, symbolName)
            val pushArgs = pushArguments(m)
            val argsSize = m.args.size
            s"""
$pushArgs
$call
add esp, ${argsSize * 4} ; remove params from stack
            """
          }
          case None =>
            throw new SyntaxError("Could not resolve method: " + an)
          case _ =>
            throw new SyntaxError("Method resolved to an abstract method declaration: " + an)
        }

      }

      case b: Block => {
        b.statements
         .map(generateAssemblyForNode(_))
         .filter{_ != ""}
         .mkString("\n")
      }

      case r: ReturnStatement => {
        val expr = r.expression match {
          case Some(e: Expression) => generateAssemblyForNode(e)
          case None => ""
        }
        expr +  """
    mov esp, ebp   ; reset the stack pointer
    pop ebp
    ret
"""
      }

      case l: LocalVariableDeclaration => {
        val exprAsm = generateAssemblyForNode(l.expression)
        s"""
$exprAsm
mov ${l.symbolName}, eax
"""
      }

      //  This is a read of an expressionname
      case e: ExpressionName => {
        e.scope.get.lookup(EnvironmentLookup.lookupFromName(e)) match {
          //  The following cases are triggered if the target of this expression is unqualified.
          case Some(l: LocalVariableDeclaration) => s"mov eax, ${l.symbolName}\n"
          case Some(f: ForVariableDeclaration) => s"mov eax, ${f.symbolName}\n"
          case Some(f: FieldDeclaration) => {
            if (f.isStatic) {
              val classDeclaration = f.scope.get.compilationScope.get.node.get.asInstanceOf[CompilationUnit].typeDeclaration.asInstanceOf[ClassDeclaration]
              s"mov eax, [${NASMDefines.VTableStaticFieldTag(classDeclaration.symbolName, f.symbolName)}]\n"
            } else {
              s"""
${if (!hasDereferencedPrefix) "mov eax, [ebp + 8]; load the \"this\" pointer into eax" else "; not loading \"this\" pointer into eax"}
cmp eax, 0; null pointer access on field declaration
je __exception
mov eax, [eax + ${(getOffsetOfInstanceField(f) + 2) * 4}]; field lookup ${f.symbolName} on eax
"""
            }
          }
          case Some(f: FormalParameter) => s"mov eax, ${f.symbolName}_${f.hashCode}; reference parameter\n"

          case None => {
            var disambiguated: ExpressionName = e
            if (e.isAmbiguous) {
              disambiguated = NameLinker.disambiguateName(e)(e.scope.get).asInstanceOf[ExpressionName]
            }

            if (disambiguated.prefix.isEmpty) {
              if (disambiguated.value.value.equals("length")) {
                //  We must have an array type here
                s"""
cmp eax, 0; null pointer access on array length
  je __exception
mov eax, [eax + ArrayLengthOffset]
"""
              } else {
                throw new SyntaxError("Could not resolve expressionname for expression evaluation.")
              }
            } else if (disambiguated.prefix.get.isInstanceOf[ExpressionName]) {
              // Look up the expression name of the prefix and the name separately.
              val endValue = ExpressionName(disambiguated.value)
              endValue.scope = e.scope
              generateAssemblyForNode(disambiguated.prefix.get) + generateAssemblyForNode(endValue)(
                parentCompilationUnit,
                parentClassDeclaration,
                hasDereferencedPrefix=true
              )
            } else {
              //  The prefix is a TypeName, so we need to do a TypeName lookup to get the class
              //  then get the static field on that class. It must be static if we're here. (I think.)
              disambiguated.prefix.get.scope.get.lookup(EnvironmentLookup.lookupFromName(disambiguated.prefix.get)) match {
                case Some(c: ClassDeclaration) => {
                  val endValue = ExpressionName(disambiguated.value)
                  endValue.scope = e.scope

                  c.body.scope.get.lookup(EnvironmentLookup.lookupFromName(endValue)) match {
                    case Some(f: FieldDeclaration) if f.isStatic => {
                      s"mov eax, [${NASMDefines.VTableStaticFieldTag(c.symbolName, f.symbolName)}]; static field ref\n"
                    }
                    case _ => throw new SyntaxError("Could not find static field decl in class")
                  }
                }
                case _ => throw new SyntaxError("Could not find typename for expression reference")
              }
            }
          }

          case x =>
            throw new SyntaxError("Environment lookup for name " + e.niceName + " resulted in unknown node " + x)
        }
      }

      case s: SimpleArrayAccess => {
        val arrayAsm:String = generateAssemblyForNode(s.name)
        val indexAsm:String = generateAssemblyForNode(s.expr)
        valueAtArrayIndex(arrayAsm, indexAsm)
      }

      case c: ComplexArrayAccess => {
        val arrayAsm:String = generateAssemblyForNode(c.primary)
        val indexAsm:String = generateAssemblyForNode(c.expr)
        valueAtArrayIndex(arrayAsm, indexAsm)
      }

      case a: Assignment => {
        val rhsAsm: String = generateAssemblyForNode(a.rightHandSide)
        val lhsAsm: String = a.leftHandSide match {
          case f: FieldAccess => ""

          // This is a write to an ExpressionName
          case e: ExpressionName => e.scope.get.lookup(EnvironmentLookup.lookupFromName(NameLinker.disambiguateName(e)(e.scope.get))) match {
            case Some(l: LocalVariableDeclaration) => s"mov ${l.symbolName}, eax\n"
            case Some(f: ForVariableDeclaration) => s"mov ${f.symbolName}, eax\n"
            case Some(f: FieldDeclaration) => {
              if (f.isStatic) {
                val classDeclaration = f.scope.get.compilationScope.get.node.get.asInstanceOf[CompilationUnit].typeDeclaration.asInstanceOf[ClassDeclaration]
                s"mov [${NASMDefines.VTableStaticFieldTag(classDeclaration.symbolName, f.symbolName)}], eax\n"
              } else {
                s"mov [${(getOffsetOfInstanceField(f) + 2) * 4}], eax; field declaration assignment\n"
              }
            }
            case Some(f: FormalParameter) => s"mov ${f.symbolName}_${f.hashCode}, eax\n"

            case None => {
              var disambiguated: ExpressionName = e
              if (e.isAmbiguous) {
                disambiguated = NameLinker.disambiguateName(e)(e.scope.get).asInstanceOf[ExpressionName]
              }

              if (disambiguated.prefix.isEmpty) {
                throw new SyntaxError("Could not resolve expressionname for assignment.")
              } else {
                //  TODO: Verify that assignment to something like System.out.field works

                // Look up the expression name of the prefix and the name separately.
                val endValue = ExpressionName(disambiguated.value)
                endValue.scope = e.scope

                e.scope.get.lookup(EnvironmentLookup.lookupFromName(endValue)) match {
                  case Some(f: FieldDeclaration) => {
                    if (f.isStatic) {
                      val classDeclaration = f.scope.get.compilationScope.get.node.get.asInstanceOf[CompilationUnit].typeDeclaration.asInstanceOf[ClassDeclaration]
s"""
${generateAssemblyForNode(disambiguated.prefix.get)}
mov [${NASMDefines.VTableStaticFieldTag(classDeclaration.symbolName, f.symbolName)}], eax
"""
                    } else {
                      s"""
push eax
${generateAssemblyForNode(disambiguated.prefix.get)}
pop ebx ; ebx now contains the RHS
mov [eax + ${(getOffsetOfInstanceField(f) + 2) * 4}], ebx; field declaration assignment
mov eax, ebx ; assignment should return the result
"""
                    }
                  }
                  case _
                    => throw new SyntaxError("Could not find instance field to assign to")
                }
              }
            }

            case x =>
              throw new SyntaxError("Environment lookup for name " + e.niceName + " resulted in unknown node " + x)
          }

          case s: SimpleArrayAccess => {
            val arrayAsm:String = generateAssemblyForNode(s.name)
            val indexAsm:String = generateAssemblyForNode(s.expr)
            assignToArrayIndex(arrayAsm, indexAsm)
          }

          case c: ComplexArrayAccess => {
            val arrayAsm:String = generateAssemblyForNode(c.primary)
            val indexAsm:String = generateAssemblyForNode(c.expr)
            assignToArrayIndex(arrayAsm, indexAsm)
          }

          case x
            => throw new SyntaxError("Cannot assign to " + x)
        }

        s"""
$rhsAsm
$lhsAsm
        """
      }

      case n: Num => s"mov eax, ${n.value}\n"

      case _: NullLiteral => s"mov eax, 0\n"
      case _: FalseLiteral => s"mov eax, 0\n"
      case _: TrueLiteral => s"mov eax, 1\n"

      case s: StringLiteral =>
        s"""
; string literal "${s.stringVal}"
mov eax, _string_literal_${s.index.get}_String
        """

      case c: CharLiteral =>
        s"""
; char literal ${c.value.value}
mov dword eax, ${c.value.value}
        """

      case AddExpression(e1, e2) => (
        generateAssemblyForNode(e1) + "push eax\n"
        + generateAssemblyForNode(e2) + "push eax\n"
        + """
pop ebx
pop eax
add eax, ebx
        """
      )
      case SubtractExpression(e1, e2) => (
        generateAssemblyForNode(e1) + "push eax\n"
        + generateAssemblyForNode(e2) + "push eax\n"
        + """
pop ebx
pop eax
sub eax, ebx
        """
      )
      case MultiplyExpression(e1, e2) => (
        generateAssemblyForNode(e1) + "push eax\n"
        + generateAssemblyForNode(e2) + "push eax\n"
        + """
pop eax
pop edx
imul edx
        """
      )
      case d : DivideExpression => {
        val lhsAsm:String = generateAssemblyForNode(d.e1)
        val rhsAsm:String = generateAssemblyForNode(d.e2)

        s"""
$lhsAsm
push eax
$rhsAsm
push eax

pop ebx
pop eax

sub ebx, 0
je __exception

cdq
idiv ebx
        """
      }

      case InstanceOfExpression(e: Expression, c: ReferenceType) => {
        //  Evaluate the expression inside, then move it into eax.
        //  If the target type is a reference, verify that the resulting object pointer is an instance of the target.
        val prepareToValidateCast = s"""
cmp eax, 0 ; if null, treat classTag as 0
je .instanceOf_${c.symbolName}_${e.symbolName}_classTagIsInEAX

; move vtable of the invocation target into ebx
mov ebx, [eax + ObjectVTableOffset]
.instanceOf_${c.symbolName}_${e.symbolName}_classTagIsInEAX:
"""
        val offsetCall = generateOffsetCallForType(c)

        val postValidateCast = s"""
; check eax to see if the cast was successful
sub eax, NoVTableOffsetFound
; if the cast was successful, eax will be nonzero
"""

        generateAssemblyForNode(e) + prepareToValidateCast + offsetCall + postValidateCast
      }

      case r :RelationalExpression => {
        var jmpAsm:String = ""

        var lhsAsm:String = ""
        var rhsAsm:String = ""

        val falseCase = r.symbolName + "_FALSE"
        val trueCase = r.symbolName + "_TRUE"
        val finalCase = r.symbolName + "_FINAL"

        r match {
          case _: InstanceOfExpression
            => throw new SyntaxError("InstanceOfExpression should not be caught here.")
          case EqualExpression(e1, e2) =>
            jmpAsm = "jz"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
          case NotEqualExpression(e1, e2) =>
            jmpAsm = "jnz"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
          case LessThanExpression(e1, e2) =>
            jmpAsm = "jl"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
          case LessEqualExpression(e1, e2) =>
            jmpAsm = "jle"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
          case GreaterThanExpression(e1, e2) =>
            jmpAsm = "jg"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
          case GreaterEqualExpression(e1, e2) =>
            jmpAsm = "jge"
            lhsAsm = generateAssemblyForNode(e1)
            rhsAsm = generateAssemblyForNode(e2)
        }

        s"""
$lhsAsm
push eax

$rhsAsm
push eax

pop ebx
pop eax
sub eax, ebx

$jmpAsm $trueCase

$falseCase:
mov eax, 0
jmp $finalCase

$trueCase:
mov eax, 1

$finalCase:
        """
      }

      case NegatedExpression(expr) =>
        generateAssemblyForNode(expr) +
        """
neg eax
        """

      case LogicalNotExpression(expr) =>
        generateAssemblyForNode(expr) +
        """
neg eax
sbb eax, eax
inc eax
        """

      case c : ConditionalExpression => {
        val lhsAsm:String = generateAssemblyForNode(c.e1)
        val rhsAsm:String = generateAssemblyForNode(c.e2)

        val falseLabel:String = c.symbolName + "_FALSE"
        val trueLabel:String = c.symbolName + "_TRUE"
        val finalLabel:String = c.symbolName + "_FINAL"

        c match {
          case _:OrExpression =>
            s"""
$lhsAsm
sub eax, 0
jnz $trueLabel

$rhsAsm
sub eax, 0
jnz $trueLabel

mov eax, 0
jmp $finalLabel

$trueLabel:
mov eax, 1

$finalLabel:
            """
          case _:AndExpression =>
            s"""
$lhsAsm
sub eax, 0
jz $falseLabel

$rhsAsm
sub eax, 0
jz $falseLabel

mov eax, 1
jmp $finalLabel

$falseLabel:
mov eax, 0

$finalLabel:
            """
          case _:BinOrExpression =>
            s"""
$lhsAsm
push eax

$rhsAsm
push eax

pop ebx
pop eax
or eax, ebx
          """
          case _:BinXorExpression =>
            s"""
$lhsAsm
push eax

$rhsAsm
push eax

pop ebx
pop eax
xor eax, ebx
          """
          case _:BinAndExpression =>
            s"""
$lhsAsm
push eax

$rhsAsm
push eax

pop ebx
pop eax
and eax, ebx
          """

        }
      }

      case a : ArrayCreationPrimary => {
        val dimsAsm:String = generateAssemblyForNode(a.dimExpr)
        val arrayTag:String = NASMDefines.ClassTagForClass(arraySymbolName)
        val vtableBase:String = NASMDefines.VTableBase(arraySymbolName)
        val typeAsm:String = typeOfArray(a)

        s"""
$dimsAsm
mov ecx, eax; ecx holds the length
mov ebx, 4
imul ebx ; 4x length in eax
add eax, 16 ; extra bits plus 4x length
push ecx
call __malloc
pop ecx

mov dword [eax], $arrayTag
mov dword [eax + ObjectVTableOffset], $vtableBase
mov dword [eax + ArrayTypeOffset], $typeAsm
mov dword [eax + ArrayLengthOffset], ecx
        """
      }

      case c : ClassCreationPrimary => {
        val env = c.scope.get

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

        val fields = getInstanceFieldsForClass(classDecl)
        val allocSize = fields.size + 2 // why + 2? Header includes class tag + vtable pointer
        val vtableBase = NASMDefines.VTableBase(classDecl.symbolName)

        val classSymbolName = classDecl.symbolName
        val symbolName = constructorDecl.symbolName
        val mallocAsm = s"""
mov eax, ${allocSize * 4}
call __malloc
        """

        val instanceFieldInitialization = fields.filter(_.expression.isDefined).map(f => s"""
; instance field initialization for ${f.symbolName}
push eax
${generateAssemblyForNode(f.expression.get)}
mov ebx, eax
pop eax
mov [eax + ${(getOffsetOfInstanceField(f) + 2) * 4}], ebx
; end instance field initialization for ${f.symbolName}
""").mkString("\n")

        val thunkAsm = s"""
mov dword [eax], ${NASMDefines.ClassTagForClass(classDecl.symbolName)}
mov dword [eax + ObjectVTableOffset], $vtableBase
$instanceFieldInitialization

mov eax, [eax + ObjectVTableOffset]
        """

        val call = generateInstanceCallAssembly(mallocAsm, thunkAsm, classSymbolName, symbolName, isConstructor = true)
        val pushArgs = pushArguments(c)
        val argsSize = c.args.size

        s"""
$pushArgs
$call
add esp, ${argsSize * 4} ; remove params from the stack
        """
      }

      case i : IfStatement => {
        val clauseAsm:String = generateAssemblyForNode(i.clause)
        var trueAsm:String = ""
        var falseAsm:String = ""

        if (!i.trueCase.isEmpty) {
          trueAsm = generateAssemblyForNode(i.trueCase.get)
        }
        if (!i.elseCase.isEmpty) {
          falseAsm = generateAssemblyForNode(i.elseCase.get)
        }

        val trueLabel:String = i.symbolName + "_TRUE"
        val falseLabel:String = i.symbolName + "_FALSE"
        val finalLabel:String = i.symbolName + "_FINAL"

        s"""
$clauseAsm
sub eax, 0
jz $falseLabel

$trueLabel:
$trueAsm
jmp $finalLabel

$falseLabel:
$falseAsm

$finalLabel:
        """
      }

      case w : WhileStatement => {
        val whileCheckAsm:String = generateAssemblyForNode(w.clause)

        var whileBodyAsm:String =""
        if (!w.body.isEmpty) {
          whileBodyAsm = generateAssemblyForNode(w.body.get)
        }

        val topLabel:String =  w.symbolName + "_TOP"
        val bottomLabel:String = w.symbolName + "_BOTTOM"

        s"""
$topLabel:

; perform the check of the while loop
$whileCheckAsm
sub eax, 0
jz $bottomLabel

$whileBodyAsm

; end of while loop body
jmp $topLabel
$bottomLabel:
        """
      }

      case f : ForStatement => {
        var forInitAsm:String = ""
        var forCheckAsm:String = ""
        var forUpdateAsm:String = ""
        var forBodyAsm:String = ""

        val topLabel:String = f.symbolName + "_TOP"
        val bottomLabel:String = f.symbolName + "_BOTTOM"

        if (!f.init.isEmpty) {
          forInitAsm = generateAssemblyForNode(f.init.get)
        }

        if (!f.check.isEmpty) {
          forCheckAsm = generateAssemblyForNode(f.check.get)
        }

        if (!f.update.isEmpty) {
          forUpdateAsm = generateAssemblyForNode(f.update.get)
        }

        forBodyAsm = generateAssemblyForNode(f.statement)

        s"""
$forInitAsm

$topLabel:
; perform the check of the for loop
$forCheckAsm
sub eax, 0
jz $bottomLabel

$forBodyAsm

; at end of for loop body, do the update
$forUpdateAsm
jmp $topLabel
$bottomLabel:
        """
      }

      case c: CastExpression => {
        if (c.targetType.isInstanceOf[PrimitiveType]) {
          return "; cast validation is a nop - the target type is primitive\n" + generateAssemblyForNode(c.expr)
        }

        //  Evaluate the expression inside, then move it into eax.
        //  If the target type is a reference, verify that the resulting object pointer is an instance of the target.
        val prepareToValidateCast = s"""
; validating cast - first push the object pointer in eax to save it
push eax

cmp eax, 0 ; if null, treat classTag as 0
je .cast_${c.expr.symbolName}_${c.targetType.symbolName}_castingNull

; move vtable of the invocation target into ebx
mov ebx, [eax + ObjectVTableOffset]
"""
        val offsetCall = generateOffsetCallForType(c.targetType)

        val postValidateCast = s"""
; check eax to see if the cast was successful
cmp eax, NoVTableOffsetFound
je __exception
.cast_${c.expr.symbolName}_${c.targetType.symbolName}_castingNull:
"""

        prepareToValidateCast + offsetCall + postValidateCast + generateAssemblyForNode(c.expr)
      }

      case x => x.children.map(generateAssemblyForNode).filter{_ != ""}.mkString("\n")
    }

    s"""
; begin asm for node ${n.getClass.getSimpleName} 0x${n.hashCode.toHexString}
$asm
; end asm for node ${n.getClass.getSimpleName} 0x${n.hashCode.toHexString}
    """
  }

  def assignToArrayIndex(arrayAsm:String, indexAsm:String) : String = {
    // Relies on rhs value of assignment to be in eax
            s"""
push eax ; push rhs value

$arrayAsm
push eax
$indexAsm
push eax

pop ecx
pop ebx
pop eax

cmp ebx, 0
je __exception

cmp ecx, [ebx + ArrayLengthOffset]
jge __exception

cmp ecx, 0
jl __exception

mov dword [ebx + ArrayLengthOffset + 4*(ecx + 1)], eax
            """
  }

  def valueAtArrayIndex(arrayAsm:String, indexAsm:String) : String = {
    s"""
$arrayAsm
push eax

$indexAsm
push eax

pop ebx
pop eax

cmp eax, 0
je __exception

cmp ebx, [eax + ArrayLengthOffset]
jge __exception

cmp ebx, 0
jl __exception

mov eax, [eax + ArrayLengthOffset + 4*(ebx + 1)]
            """
  }


  def typeOfArray(a: ArrayCreationPrimary) : String = {
    a.varType match {
      case p:PrimitiveType =>
        p match {
          case BooleanKeyword() => "BooleanTypeTag"
          case ByteKeyword() => "ByteTypeTag"
          case ShortKeyword() => "ShortTypeTag"
          case IntKeyword() => "IntTypeTag"
          case CharKeyword() => "CharTypeTag"
        }
      case t:ReferenceNonArrayType => NASMDefines.ClassTagForClass(t.node.get.symbolName)
      case _ => throw new SyntaxError("Invalid var type of array.")
    }
  }

  def pushArguments(node:AbstractSyntaxNode) : String = {
    val args : Seq[Expression] = node match {
      case smi: SimpleMethodInvocation => smi.args
      case cmi: ComplexMethodInvocation =>   cmi.args
      case ccp: ClassCreationPrimary => ccp.args
      case x => throw new SyntaxError("Cannot push arguments for node type without arguments: " + x)
    }

    args.map(a => {
      generateAssemblyForNode(a)(None,None) + "\npush eax; argument for call\n"
    }).mkString("\n")
  }

  def generateInstanceCallAssembly(instanceAsm:String, thunkAsm:String, className:String, callName:String, isConstructor: Boolean = false) : String = {
    s"""
$instanceAsm
cmp eax, 0; null pointer access on instance call
je __exception

push eax
$thunkAsm
${NASMDefines.VMethodCall("eax", className, callName)}
${if (isConstructor) { "pop eax; remove this from the stack and put into eax" } else { "add esp, 4 ; remove the this from the stack" }}
    """
  }
}
