package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import java.io.File
import joosbox.parser.ExpressionNameLookup
import joosbox.parser.AbstractSyntaxNode.QualifiedName
import joosbox.lexer.InputString

class FailTypeCheckerSpec extends Specification {
  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a3/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  "Type Checker" should {
      "environment equality" in {
        val a = ExpressionNameLookup(QualifiedName(Seq(InputString("a", "abc", 123, 234))))
        val b = ExpressionNameLookup(QualifiedName(Seq(InputString("a", "def", 345, 567))))
        a mustEqual b
      }

      "environment map" in {

        val a = ExpressionNameLookup(QualifiedName(Seq(InputString("a", "abc", 123, 234))))
        val map = Map(a -> true)

        val b = ExpressionNameLookup(QualifiedName(Seq(InputString("a", "def", 345, 567))))
        map.get(b) must beEqualTo(Some(true))
      }

      "J1_ArrayInterfaces.java" in {
        val files = filesForTest("J1_ArrayInterfaces.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray)// must not(throwA[Exception])
        true
      }
  }
}

class TypeCheckerSpec extends Specification {

  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a3/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  "Compiler" should {
    "pass marmoset tests for assignment 3" in {
      "J1_5_AmbiguousName_DefaultPackageNotVisible" in {
        val files = filesForTest("J1_5_AmbiguousName_DefaultPackageNotVisible") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must  not(throwA[Exception])
      }
      "J1_5_AmbiguousName_FieldVsType.java" in {
        val files = filesForTest("J1_5_AmbiguousName_FieldVsType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_AmbiguousName_FieldVsType_Initializer.java" in {
        val files = filesForTest("J1_5_AmbiguousName_FieldVsType_Initializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_AmbiguousName_LocalVsField.java" in {
        val files = filesForTest("J1_5_AmbiguousName_LocalVsField.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_AmbiguousName_LocalVsField_SameLine.java" in {
        val files = filesForTest("J1_5_AmbiguousName_LocalVsField_SameLine.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_AmbiguousName_LocalVsType.java" in {
        val files = filesForTest("J1_5_AmbiguousName_LocalVsType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_ForwardReference_ArrayLength.java" in {
        val files = filesForTest("J1_5_ForwardReference_ArrayLength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_ForwardReference_EqualInfix.java" in {
        val files = filesForTest("J1_5_ForwardReference_EqualInfix.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_ForwardReference_ExplicitThis_InAssignment.java" in {
        val files = filesForTest("J1_5_ForwardReference_ExplicitThis_InAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_5_ForwardReference_SameLine.java" in {
        val files = filesForTest("J1_5_ForwardReference_SameLine.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_Assignable_Object_ObjectArray.java" in {
        val files = filesForTest("J1_6_Assignable_Object_ObjectArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_AssignmentInArrayLength.java" in {
        val files = filesForTest("J1_6_AssignmentInArrayLength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_AssignmentInNotArrayLength.java" in {
        val files = filesForTest("J1_6_AssignmentInNotArrayLength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_ImplicitSuper" in {
        val files = filesForTest("J1_6_ProtectedAccess_ImplicitSuper") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceField_SubVar" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceField_SubVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceField_This" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceField_This") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceField_ThisVar" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceField_ThisVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceMethod_SubVar" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceMethod_SubVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceMethod_This" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceMethod_This") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_InstanceMethod_ThisVar" in {
        val files = filesForTest("J1_6_ProtectedAccess_InstanceMethod_ThisVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_StaticMethod_Sub" in {
        val files = filesForTest("J1_6_ProtectedAccess_StaticMethod_Sub") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_StaticMethod_Super" in {
        val files = filesForTest("J1_6_ProtectedAccess_StaticMethod_Super") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_ProtectedAccess_StaticMethod_This" in {
        val files = filesForTest("J1_6_ProtectedAccess_StaticMethod_This") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_6_StaticMethodCall_ThisInArg.java" in {
        val files = filesForTest("J1_6_StaticMethodCall_ThisInArg.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_A_ConcatInSimpleInvoke.java" in {
        val files = filesForTest("J1_A_ConcatInSimpleInvoke.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_A_ConcatInStaticInvoke.java" in {
        val files = filesForTest("J1_A_ConcatInStaticInvoke.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_accessstaticfield" in {
        val files = filesForTest("J1_accessstaticfield") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ambiguousInvoke" in {
        val files = filesForTest("J1_ambiguousInvoke") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_array.java" in {
        val files = filesForTest("J1_array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_arrayAccess.java" in {
        val files = filesForTest("J1_arrayAccess.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayAccess_Cast.java" in {
        val files = filesForTest("J1_ArrayAccess_Cast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayAccess_MethodInvocation.java" in {
        val files = filesForTest("J1_ArrayAccess_MethodInvocation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayCast.java" in {
        val files = filesForTest("J1_ArrayCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayCast1" in {
        val files = filesForTest("J1_ArrayCast1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayCast2" in {
        val files = filesForTest("J1_ArrayCast2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayCast3" in {
        val files = filesForTest("J1_ArrayCast3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayCast4" in {
        val files = filesForTest("J1_ArrayCast4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_arrayinstanceof1.java" in {
        val files = filesForTest("J1_arrayinstanceof1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_arrayinstanceof2.java" in {
        val files = filesForTest("J1_arrayinstanceof2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ArrayInterfaces.java" in {
        val files = filesForTest("J1_ArrayInterfaces.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_arraylength.java" in {
        val files = filesForTest("J1_arraylength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_assign_Object_to_Object.java" in {
        val files = filesForTest("J1_assign_Object_to_Object.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_backwardRef.java" in {
        val files = filesForTest("J1_backwardRef.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_backwardsFieldRef.java" in {
        val files = filesForTest("J1_backwardsFieldRef.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_BigByteInit.java" in {
        val files = filesForTest("J1_BigByteInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_BigCharCharInit.java" in {
        val files = filesForTest("J1_BigCharCharInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_BigShortFromByteInit.java" in {
        val files = filesForTest("J1_BigShortFromByteInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_BigShortInit.java" in {
        val files = filesForTest("J1_BigShortInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_boolean.java" in {
        val files = filesForTest("J1_boolean.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_booleanliterals.java" in {
        val files = filesForTest("J1_booleanliterals.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_byte.java" in {
        val files = filesForTest("J1_byte.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ByteCast.java" in {
        val files = filesForTest("J1_ByteCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ByteCharInit2.java" in {
        val files = filesForTest("J1_ByteCharInit2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ByteInit.java" in {
        val files = filesForTest("J1_ByteInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_callstaticmethods.java" in {
        val files = filesForTest("J1_callstaticmethods.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_cast_to_same_type.java" in {
        val files = filesForTest("J1_cast_to_same_type.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castarrayaccess.java" in {
        val files = filesForTest("J1_castarrayaccess.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castMultiple.java" in {
        val files = filesForTest("J1_castMultiple.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castMultiple1.java" in {
        val files = filesForTest("J1_castMultiple1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castMultiple2.java" in {
        val files = filesForTest("J1_castMultiple2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castprimarymethodinvocation.java" in {
        val files = filesForTest("J1_castprimarymethodinvocation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_castthis.java" in {
        val files = filesForTest("J1_castthis.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_CharCast.java" in {
        val files = filesForTest("J1_CharCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_CharCharInit1.java" in {
        val files = filesForTest("J1_CharCharInit1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_closestMatchConstructor1.java" in {
        val files = filesForTest("J1_closestMatchConstructor1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ClosestMatchMultiplePath1.java" in {
        val files = filesForTest("J1_ClosestMatchMultiplePath1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ClosestMatchMultiplePath2.java" in {
        val files = filesForTest("J1_ClosestMatchMultiplePath2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ClosestMethod2.java" in {
        val files = filesForTest("J1_ClosestMethod2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ClosestMethod3.java" in {
        val files = filesForTest("J1_ClosestMethod3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ClosestMethod4.java" in {
        val files = filesForTest("J1_ClosestMethod4.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_constructoroverloading.java" in {
        val files = filesForTest("J1_constructoroverloading.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromArray" in {
        val files = filesForTest("J1_evalMethodInvocationFromArray") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromLit" in {
        val files = filesForTest("J1_evalMethodInvocationFromLit") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromMethod" in {
        val files = filesForTest("J1_evalMethodInvocationFromMethod") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromObject" in {
        val files = filesForTest("J1_evalMethodInvocationFromObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromParExp.java" in {
        val files = filesForTest("J1_evalMethodInvocationFromParExp.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_evalMethodInvocationFromThis" in {
        val files = filesForTest("J1_evalMethodInvocationFromThis") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldinit.java" in {
        val files = filesForTest("J1_fieldinit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldinit2.java" in {
        val files = filesForTest("J1_fieldinit2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldinit_forward_ref.java" in {
        val files = filesForTest("J1_fieldinit_forward_ref.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldinit_forward_ref2.java" in {
        val files = filesForTest("J1_fieldinit_forward_ref2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldInOwnInit.java" in {
        val files = filesForTest("J1_fieldInOwnInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldOwnInit1.java" in {
        val files = filesForTest("J1_fieldOwnInit1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_fieldOwnInit2.java" in {
        val files = filesForTest("J1_fieldOwnInit2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_FieldRestrictionDuringInit.java" in {
        val files = filesForTest("J1_FieldRestrictionDuringInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_formal_with_same_name_as_field.java" in {
        val files = filesForTest("J1_formal_with_same_name_as_field.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_formalindex.java" in {
        val files = filesForTest("J1_formalindex.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_forwardfield1.java" in {
        val files = filesForTest("J1_forwardfield1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_forwardfield2.java" in {
        val files = filesForTest("J1_forwardfield2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_good_dot.java" in {
        val files = filesForTest("J1_good_dot.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_implicitstringconcatenation.java" in {
        val files = filesForTest("J1_implicitstringconcatenation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_implicitthisforfields.java" in {
        val files = filesForTest("J1_implicitthisforfields.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_implicitthisformethods.java" in {
        val files = filesForTest("J1_implicitthisformethods.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_instanceof.java" in {
        val files = filesForTest("J1_instanceof.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_instanceof_array.java" in {
        val files = filesForTest("J1_instanceof_array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_instanceof_array2.java" in {
        val files = filesForTest("J1_instanceof_array2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_instanceof_string.java" in {
        val files = filesForTest("J1_instanceof_string.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_int.java" in {
        val files = filesForTest("J1_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_interface_null" in {
        val files = filesForTest("J1_interface_null") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_interfaceassignable" in {
        val files = filesForTest("J1_interfaceassignable") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_InterfaceObject" in {
        val files = filesForTest("J1_InterfaceObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_intstringadd.java" in {
        val files = filesForTest("J1_intstringadd.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_length_field_not_array.java" in {
        val files = filesForTest("J1_length_field_not_array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_localDeclAccess.java" in {
        val files = filesForTest("J1_localDeclAccess.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_methodInvocationQualified.java" in {
        val files = filesForTest("J1_methodInvocationQualified.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_methodoverloading.java" in {
        val files = filesForTest("J1_methodoverloading.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_methodWithArgList.java" in {
        val files = filesForTest("J1_methodWithArgList.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_NamedCast2" in {
        val files = filesForTest("J1_NamedCast2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_NamedCast3" in {
        val files = filesForTest("J1_NamedCast3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_NamedCast4" in {
        val files = filesForTest("J1_NamedCast4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_namelinking3.java" in {
        val files = filesForTest("J1_namelinking3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_nestedblocks.java" in {
        val files = filesForTest("J1_nestedblocks.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_nestedcast.java" in {
        val files = filesForTest("J1_nestedcast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_nonthisfieldaccess.java" in {
        val files = filesForTest("J1_nonthisfieldaccess.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_nullinstanceof1.java" in {
        val files = filesForTest("J1_nullinstanceof1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_OneByteByteCast.java" in {
        val files = filesForTest("J1_OneByteByteCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_OneByteCharCast.java" in {
        val files = filesForTest("J1_OneByteCharCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_OneByteIntCast.java" in {
        val files = filesForTest("J1_OneByteIntCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_OneByteShortCast.java" in {
        val files = filesForTest("J1_OneByteShortCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_primitivecasts.java" in {
        val files = filesForTest("J1_primitivecasts.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ProtectedAccess1" in {
        val files = filesForTest("J1_ProtectedAccess1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ProtectedAccess2" in {
        val files = filesForTest("J1_ProtectedAccess2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ProtectedAccess4" in {
        val files = filesForTest("J1_ProtectedAccess4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_referencecasts.java" in {
        val files = filesForTest("J1_referencecasts.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_samestaticinvoketwice.java" in {
        val files = filesForTest("J1_samestaticinvoketwice.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_short.java" in {
        val files = filesForTest("J1_short.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ShortCast.java" in {
        val files = filesForTest("J1_ShortCast.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ShortCharInit2.java" in {
        val files = filesForTest("J1_ShortCharInit2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ShortFromByteInit.java" in {
        val files = filesForTest("J1_ShortFromByteInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_ShortInit.java" in {
        val files = filesForTest("J1_ShortInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_sideeffects_obj3.java" in {
        val files = filesForTest("J1_sideeffects_obj3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_StaticField_AccessFromClass.java" in {
        val files = filesForTest("J1_StaticField_AccessFromClass.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_staticMethodInvocation.java" in {
        val files = filesForTest("J1_staticMethodInvocation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_supermethod_override11" in {
        val files = filesForTest("J1_supermethod_override11") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_array.java" in {
        val files = filesForTest("J1_typecheck_array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_assignment" in {
        val files = filesForTest("J1_typecheck_assignment") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_constructor_invocation.java" in {
        val files = filesForTest("J1_typecheck_constructor_invocation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_equality.java" in {
        val files = filesForTest("J1_typecheck_equality.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_expstm.java" in {
        val files = filesForTest("J1_typecheck_expstm.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_if1.java" in {
        val files = filesForTest("J1_typecheck_if1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_if2.java" in {
        val files = filesForTest("J1_typecheck_if2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof.java" in {
        val files = filesForTest("J1_typecheck_instanceof.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof1.java" in {
        val files = filesForTest("J1_typecheck_instanceof1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof2.java" in {
        val files = filesForTest("J1_typecheck_instanceof2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof3.java" in {
        val files = filesForTest("J1_typecheck_instanceof3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof4.java" in {
        val files = filesForTest("J1_typecheck_instanceof4.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof5.java" in {
        val files = filesForTest("J1_typecheck_instanceof5.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof6.java" in {
        val files = filesForTest("J1_typecheck_instanceof6.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_instanceof7.java" in {
        val files = filesForTest("J1_typecheck_instanceof7.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_plus.java" in {
        val files = filesForTest("J1_typecheck_plus.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_return.java" in {
        val files = filesForTest("J1_typecheck_return.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_static_invocation1.java" in {
        val files = filesForTest("J1_typecheck_static_invocation1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_typecheck_while.java" in {
        val files = filesForTest("J1_typecheck_while.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J1_wrapper_classes_eq.java" in {
        val files = filesForTest("J1_wrapper_classes_eq.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_5_ForwardReference_StaticField.java" in {
        val files = filesForTest("J2_5_ForwardReference_StaticField.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_6_AmbiguousName_StaticFieldAccess" in {
        val files = filesForTest("J2_6_AmbiguousName_StaticFieldAccess") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_6_ProtectedAccess_StaticField_Sub" in {
        val files = filesForTest("J2_6_ProtectedAccess_StaticField_Sub") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_6_ProtectedAccess_StaticField_Super" in {
        val files = filesForTest("J2_6_ProtectedAccess_StaticField_Super") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_6_ProtectedAccess_StaticField_This" in {
        val files = filesForTest("J2_6_ProtectedAccess_StaticField_This") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_backwardsStaticFieldRef.java" in {
        val files = filesForTest("J2_backwardsStaticFieldRef.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_exactMatchConstructor3.java" in {
        val files = filesForTest("J2_exactMatchConstructor3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_fieldinit_forward_ref.java" in {
        val files = filesForTest("J2_fieldinit_forward_ref.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_forwardRef.java" in {
        val files = filesForTest("J2_forwardRef.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_implicitStaticMethod" in {
        val files = filesForTest("J2_implicitStaticMethod") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_interfaces" in {
        val files = filesForTest("J2_interfaces") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_ProtectedAccess3" in {
        val files = filesForTest("J2_ProtectedAccess3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_static_decl.java" in {
        val files = filesForTest("J2_static_decl.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_static_shared.java" in {
        val files = filesForTest("J2_static_shared.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_staticField.java" in {
        val files = filesForTest("J2_staticField.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_staticField2.java" in {
        val files = filesForTest("J2_staticField2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "J2_staticFieldDecl.java" in {
        val files = filesForTest("J2_staticFieldDecl.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }
      "Je_16_ClosestMatch_Array.java" in {
        val files = filesForTest("Je_16_ClosestMatch_Array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_ClosestMatch_Constructor_NoClosestMatch_This.java" in {
        val files = filesForTest("Je_16_ClosestMatch_Constructor_NoClosestMatch_This.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_ArrayLengthDec.java" in {
        val files = filesForTest("Je_16_IncDec_Final_ArrayLengthDec.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_ArrayLengthInc.java" in {
        val files = filesForTest("Je_16_IncDec_Final_ArrayLengthInc.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_PostDec.java" in {
        val files = filesForTest("Je_16_IncDec_Final_PostDec.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_PostInc.java" in {
        val files = filesForTest("Je_16_IncDec_Final_PostInc.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_PreDec.java" in {
        val files = filesForTest("Je_16_IncDec_Final_PreDec.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_Final_PreInc.java" in {
        val files = filesForTest("Je_16_IncDec_Final_PreInc.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_StringPostDec.java" in {
        val files = filesForTest("Je_16_IncDec_StringPostDec.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_StringPostInc.java" in {
        val files = filesForTest("Je_16_IncDec_StringPostInc.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_StringPreDec.java" in {
        val files = filesForTest("Je_16_IncDec_StringPreDec.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_IncDec_StringPreInc.java" in {
        val files = filesForTest("Je_16_IncDec_StringPreInc.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_MethodPresent_WrongName_Array.java" in {
        val files = filesForTest("Je_16_MethodPresent_WrongName_Array.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_MultiArrayCreation_Assign_1.java" in {
        val files = filesForTest("Je_16_MultiArrayCreation_Assign_1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_MultiArrayCreation_Null.java" in {
        val files = filesForTest("Je_16_MultiArrayCreation_Null.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_ProtectedAccess_StaticField_Sub_DeclaredInSub" in {
        val files = filesForTest("Je_16_ProtectedAccess_StaticField_Sub_DeclaredInSub") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_StaticThis_StaticFieldInitializer.java" in {
        val files = filesForTest("Je_16_StaticThis_StaticFieldInitializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_SuperThis_InvalidSuperParameter.java" in {
        val files = filesForTest("Je_16_SuperThis_InvalidSuperParameter.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_16_SuperThis_InvalidThisParameter.java" in {
        val files = filesForTest("Je_16_SuperThis_InvalidThisParameter.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_Cast_NamedCastNegativeint.java" in {
        val files = filesForTest("Je_1_Cast_NamedCastNegativeint.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_Complement_OfIntLiteral.java" in {
        val files = filesForTest("Je_1_Complement_OfIntLiteral.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_Dot_ParenthesizedType_Field.java" in {
        val files = filesForTest("Je_1_Dot_ParenthesizedType_Field.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_Dot_ParenthesizedType_Method.java" in {
        val files = filesForTest("Je_1_Dot_ParenthesizedType_Method.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_InstanceOf_Primitive.java" in {
        val files = filesForTest("Je_1_InstanceOf_Primitive.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_1_MethodInvocation_Primitive.java" in {
        val files = filesForTest("Je_1_MethodInvocation_Primitive.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_2_Cast_NegativeComplexExpressionToNamedType.java" in {
        val files = filesForTest("Je_2_Cast_NegativeComplexExpressionToNamedType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_2_Cast_NegativeToNamedType.java" in {
        val files = filesForTest("Je_2_Cast_NegativeToNamedType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_2_Cast_NegativeToQualifiedNamedType.java" in {
        val files = filesForTest("Je_2_Cast_NegativeToQualifiedNamedType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_2_For_LocalVarInUpdate.java" in {
        val files = filesForTest("Je_2_For_LocalVarInUpdate.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_3_Resolve_LinkToCorrectPackage" in {
        val files = filesForTest("Je_3_Resolve_LinkToCorrectPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousInvoke_LocalInOwnInitializer.java" in {
        val files = filesForTest("Je_5_AmbiguousInvoke_LocalInOwnInitializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousInvoke_Static_TypeNonExisting.java" in {
        val files = filesForTest("Je_5_AmbiguousInvoke_Static_TypeNonExisting.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_DefaultPackageNotVisible" in {
        val files = filesForTest("Je_5_AmbiguousName_DefaultPackageNotVisible") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_FieldVsType_Initializer.java" in {
        val files = filesForTest("Je_5_AmbiguousName_FieldVsType_Initializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_LinkToFirstFound" in {
        val files = filesForTest("Je_5_AmbiguousName_LinkToFirstFound") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_Local_UseBeforeDeclare.java" in {
        val files = filesForTest("Je_5_AmbiguousName_Local_UseBeforeDeclare.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_NoDeclaration.java" in {
        val files = filesForTest("Je_5_AmbiguousName_NoDeclaration.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_AmbiguousName_SamePackageAndClassName.java" in {
        val files = filesForTest("Je_5_AmbiguousName_SamePackageAndClassName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_ArrayLength.java" in {
        val files = filesForTest("Je_5_ForwardReference_ArrayLength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldDeclaredLater.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldDeclaredLater.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldDeclaredLater_ComplexExp.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldDeclaredLater_ComplexExp.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldInOwnInitializer_ComplexExpression.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldInOwnInitializer_ComplexExpression.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldInOwnInitializer_Direct.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldInOwnInitializer_Direct.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldInOwnInitializer_ReadAfterAssignment.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldInOwnInitializer_ReadAfterAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_FieldInOwnInitializer_RightSideOfAssignment.java" in {
        val files = filesForTest("Je_5_ForwardReference_FieldInOwnInitializer_RightSideOfAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_InAssignment.java" in {
        val files = filesForTest("Je_5_ForwardReference_InAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_5_ForwardReference_MethodCall.java" in {
        val files = filesForTest("Je_5_ForwardReference_MethodCall.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Array_NonNumericIndex.java" in {
        val files = filesForTest("Je_6_Array_NonNumericIndex.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Array_NullTypeIndex.java" in {
        val files = filesForTest("Je_6_Array_NullTypeIndex.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ArrayLength_Invoke.java" in {
        val files = filesForTest("Je_6_ArrayLength_Invoke.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Array_Object.java" in {
        val files = filesForTest("Je_6_Assignable_Array_Object.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_byte_char.java" in {
        val files = filesForTest("Je_6_Assignable_byte_char.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_byte_int.java" in {
        val files = filesForTest("Je_6_Assignable_byte_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_byteArray_intArray.java" in {
        val files = filesForTest("Je_6_Assignable_byteArray_intArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Cast_intArray_int.java" in {
        val files = filesForTest("Je_6_Assignable_Cast_intArray_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_char_byte_1.java" in {
        val files = filesForTest("Je_6_Assignable_char_byte_1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_char_byte_2.java" in {
        val files = filesForTest("Je_6_Assignable_char_byte_2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_char_int.java" in {
        val files = filesForTest("Je_6_Assignable_char_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Condition.java" in {
        val files = filesForTest("Je_6_Assignable_Condition.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Condition_SimpleType.java" in {
        val files = filesForTest("Je_6_Assignable_Condition_SimpleType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Condition_While.java" in {
        val files = filesForTest("Je_6_Assignable_Condition_While.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Instanceof_Result.java" in {
        val files = filesForTest("Je_6_Assignable_Instanceof_Result.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Instanceof_SimpleType.java" in {
        val files = filesForTest("Je_6_Assignable_Instanceof_SimpleType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Instanceof_SimpleTypeOfSimpleType.java" in {
        val files = filesForTest("Je_6_Assignable_Instanceof_SimpleTypeOfSimpleType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_int_intArray.java" in {
        val files = filesForTest("Je_6_Assignable_int_intArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_int_null.java" in {
        val files = filesForTest("Je_6_Assignable_int_null.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_intArray_byteArray.java" in {
        val files = filesForTest("Je_6_Assignable_intArray_byteArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_intArray_int.java" in {
        val files = filesForTest("Je_6_Assignable_intArray_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_NamedCastOfComplement.java" in {
        val files = filesForTest("Je_6_Assignable_NamedCastOfComplement.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_NonstaticField.java" in {
        val files = filesForTest("Je_6_Assignable_NonstaticField.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_RefType_RefTypeArray.java" in {
        val files = filesForTest("Je_6_Assignable_RefType_RefTypeArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ResultTypeOfAssignment.java" in {
        val files = filesForTest("Je_6_Assignable_ResultTypeOfAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Return_ToSubType.java" in {
        val files = filesForTest("Je_6_Assignable_Return_ToSubType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Return_Void.java" in {
        val files = filesForTest("Je_6_Assignable_Return_Void.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_Return_VoidInVoidMethod.java" in {
        val files = filesForTest("Je_6_Assignable_Return_VoidInVoidMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ReturnInElse.java" in {
        val files = filesForTest("Je_6_Assignable_ReturnInElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_short_char.java" in {
        val files = filesForTest("Je_6_Assignable_short_char.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_short_int.java" in {
        val files = filesForTest("Je_6_Assignable_short_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ToSubtype" in {
        val files = filesForTest("Je_6_Assignable_ToSubtype") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ToSubtype_DeclInit" in {
        val files = filesForTest("Je_6_Assignable_ToSubtype_DeclInit") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ToSubtype_FieldInit.java" in {
        val files = filesForTest("Je_6_Assignable_ToSubtype_FieldInit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Assignable_ValueReturn_InConstructor.java" in {
        val files = filesForTest("Je_6_Assignable_ValueReturn_InConstructor.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_BinopExp_LogicalBitwise.java" in {
        val files = filesForTest("Je_6_BinopExp_LogicalBitwise.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_ArrayTypes.java" in {
        val files = filesForTest("Je_6_ClosestMatch_ArrayTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_Constructor_NoClosestMatch" in {
        val files = filesForTest("Je_6_ClosestMatch_Constructor_NoClosestMatch") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_Constructor_NoClosestMatch_SimpleTypes.java" in {
        val files = filesForTest("Je_6_ClosestMatch_Constructor_NoClosestMatch_SimpleTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_MultipleClosest_1.java" in {
        val files = filesForTest("Je_6_ClosestMatch_MultipleClosest_1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_MultipleClosest_2.java" in {
        val files = filesForTest("Je_6_ClosestMatch_MultipleClosest_2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_MultipleClosest_3.java" in {
        val files = filesForTest("Je_6_ClosestMatch_MultipleClosest_3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ClosestMatch_MultipleClosest_SimpleTypes.java" in {
        val files = filesForTest("Je_6_ClosestMatch_MultipleClosest_SimpleTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Constructor_WrongName.java" in {
        val files = filesForTest("Je_6_Constructor_WrongName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_ArgumentTypeMismatch.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_ArgumentTypeMismatch.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_IllegalConversion.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_IllegalConversion.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_MultipleArgumentsOneMismatch.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_MultipleArgumentsOneMismatch.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_PresentInSubclass" in {
        val files = filesForTest("Je_6_ConstructorPresent_PresentInSubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_SameLastArg.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_SameLastArg.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_Super_NoDefault" in {
        val files = filesForTest("Je_6_ConstructorPresent_Super_NoDefault") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_TooFewArguments.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_TooFewArguments.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ConstructorPresent_TooManyArguments.java" in {
        val files = filesForTest("Je_6_ConstructorPresent_TooManyArguments.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Equality_int.java" in {
        val files = filesForTest("Je_6_Equality_int.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Equality_int_NamedType.java" in {
        val files = filesForTest("Je_6_Equality_int_NamedType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Equality_StringInteger.java" in {
        val files = filesForTest("Je_6_Equality_StringInteger.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Equality_Void.java" in {
        val files = filesForTest("Je_6_Equality_Void.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_Expression_StringConcat_Void.java" in {
        val files = filesForTest("Je_6_Expression_StringConcat_Void.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_FinalField_ArrayLength.java" in {
        val files = filesForTest("Je_6_FinalField_ArrayLength.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_For_NullInCondition.java" in {
        val files = filesForTest("Je_6_For_NullInCondition.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_InstanceOf_Primitive_1.java" in {
        val files = filesForTest("Je_6_InstanceOf_Primitive_1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_InstanceOf_Primitive_2.java" in {
        val files = filesForTest("Je_6_InstanceOf_Primitive_2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_InstanceOf_Primitive_3.java" in {
        val files = filesForTest("Je_6_InstanceOf_Primitive_3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_InstantiateAbstract.java" in {
        val files = filesForTest("Je_6_InstantiateAbstract.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_InstantiateInterface.java" in {
        val files = filesForTest("Je_6_InstantiateInterface.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodInvocation_NonJoos_ReturnType.java" in {
        val files = filesForTest("Je_6_MethodInvocation_NonJoos_ReturnType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_ArgumentTypeMismatch.java" in {
        val files = filesForTest("Je_6_MethodPresent_ArgumentTypeMismatch.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_IllegalConversion.java" in {
        val files = filesForTest("Je_6_MethodPresent_IllegalConversion.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_MultipleArgumentsOneMismatch.java" in {
        val files = filesForTest("Je_6_MethodPresent_MultipleArgumentsOneMismatch.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_Nonstatic_SameLastArg.java" in {
        val files = filesForTest("Je_6_MethodPresent_Nonstatic_SameLastArg.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_PresentInSubclass" in {
        val files = filesForTest("Je_6_MethodPresent_PresentInSubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_Static_SameLastArg.java" in {
        val files = filesForTest("Je_6_MethodPresent_Static_SameLastArg.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_TooFewArguments.java" in {
        val files = filesForTest("Je_6_MethodPresent_TooFewArguments.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_MethodPresent_TooManyArguments.java" in {
        val files = filesForTest("Je_6_MethodPresent_TooManyArguments.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_NonStaticAccessToStatic_Field.java" in {
        val files = filesForTest("Je_6_NonStaticAccessToStatic_Field.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_NonStaticAccessToStatic_Method.java" in {
        val files = filesForTest("Je_6_NonStaticAccessToStatic_Method.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_ClassCreation_Sub" in {
        val files = filesForTest("Je_6_ProtectedAccess_ClassCreation_Sub") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_ClassCreation_Super" in {
        val files = filesForTest("Je_6_ProtectedAccess_ClassCreation_Super") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_Constructor" in {
        val files = filesForTest("Je_6_ProtectedAccess_Constructor") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_External" in {
        val files = filesForTest("Je_6_ProtectedAccess_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceField_NoRelation_External" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceField_NoRelation_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceField_NoRelation_Internal" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceField_NoRelation_Internal") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceField_SubDeclare_SubVar" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceField_SubDeclare_SubVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceField_SuperVar" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceField_SuperVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceMethod_SubDeclare_SubVar" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceMethod_SubDeclare_SubVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_InstanceMethod_SuperVar" in {
        val files = filesForTest("Je_6_ProtectedAccess_InstanceMethod_SuperVar") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_Method_OutsidePackage_NotBySubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_Method_OutsidePackage_NotBySubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_Method_OutsidePackage_NotInSubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_Method_OutsidePackage_NotInSubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_ReadField_OutsidePackage_NotBySubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_ReadField_OutsidePackage_NotBySubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_ReadField_OutsidePackage_NotInSubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_ReadField_OutsidePackage_NotInSubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_StaticMethod_Sub_DeclaredInSub" in {
        val files = filesForTest("Je_6_ProtectedAccess_StaticMethod_Sub_DeclaredInSub") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_SuperConstructor_NewExp" in {
        val files = filesForTest("Je_6_ProtectedAccess_SuperConstructor_NewExp") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_TwoSubtypes" in {
        val files = filesForTest("Je_6_ProtectedAccess_TwoSubtypes") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_WriteField_OutsidePackage_NotBySubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_WriteField_OutsidePackage_NotBySubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_ProtectedAccess_WriteField_OutsidePackage_NotInSubclass" in {
        val files = filesForTest("Je_6_ProtectedAccess_WriteField_OutsidePackage_NotInSubclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticAccessToNontatic_Field.java" in {
        val files = filesForTest("Je_6_StaticAccessToNontatic_Field.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticAccessToNontatic_Method.java" in {
        val files = filesForTest("Je_6_StaticAccessToNontatic_Method.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_AfterStaticInvoke.java" in {
        val files = filesForTest("Je_6_StaticThis_AfterStaticInvoke.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_InvokeNonStatic.java" in {
        val files = filesForTest("Je_6_StaticThis_InvokeNonStatic.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_InvokeNonstatic_Implicit.java" in {
        val files = filesForTest("Je_6_StaticThis_InvokeNonstatic_Implicit.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_InvokeStatic.java" in {
        val files = filesForTest("Je_6_StaticThis_InvokeStatic.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_NonstaticField.java" in {
        val files = filesForTest("Je_6_StaticThis_NonstaticField.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StaticThis_NonStaticField_ImplicitThis.java" in {
        val files = filesForTest("Je_6_StaticThis_NonStaticField_ImplicitThis.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_6_StringMinus.java" in {
        val files = filesForTest("Je_6_StringMinus.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_AccessToStaticFieldWithImplicitThis" in {
        val files = filesForTest("Je_AccessToStaticFieldWithImplicitThis") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_BadConstructorName.java" in {
        val files = filesForTest("Je_BadConstructorName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
    }
  }
}
