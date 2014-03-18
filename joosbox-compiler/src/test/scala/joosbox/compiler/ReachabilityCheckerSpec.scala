package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import java.io.File
import joosbox.parser.ExpressionNameLookup
import joosbox.parser.AbstractSyntaxNode.QualifiedName
import joosbox.lexer.InputString

class ReachabilityCheckerSpec extends Specification {

  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a4/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  "Compiler" should {
    "pass marmoset tests for assignment 4" in {
      "J1_7_Reachability_AfterIfWithWhileTrue.java" in {
        val files = filesForTest("J1_7_Reachability_AfterIfWithWhileTrue.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_EmptyVoidMethod.java" in {
        val files = filesForTest("J1_7_Reachability_EmptyVoidMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_IfAndWhile_Return.java" in {
        val files = filesForTest("J1_7_Reachability_IfAndWhile_Return.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_IfThenElse_InConstructor.java" in {
        val files = filesForTest("J1_7_Reachability_IfThenElse_InConstructor.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_IfThenElse_InValueMethod.java" in {
        val files = filesForTest("J1_7_Reachability_IfThenElse_InValueMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_IfThenElse_InVoidMethod.java" in {
        val files = filesForTest("J1_7_Reachability_IfThenElse_InVoidMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Reachability_WhileTrue_ConstantFolding.java" in {
        val files = filesForTest("J1_7_Reachability_WhileTrue_ConstantFolding.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_7_Unreachable_IfEqualsNot.java" in {
        val files = filesForTest("J1_7_Unreachable_IfEqualsNot.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_Reachable1.java" in {
        val files = filesForTest("J1_Reachable1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_Reachable2.java" in {
        val files = filesForTest("J1_Reachable2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_Reachable3.java" in {
        val files = filesForTest("J1_Reachable3.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_Reachable4.java" in {
        val files = filesForTest("J1_Reachable4.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_Unreachable.java" in {
        val files = filesForTest("J1_Unreachable.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_arbitraryreturn.java" in {
        val files = filesForTest("J1_arbitraryreturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_defasn_use_before_declare.java" in {
        val files = filesForTest("J1_defasn_use_before_declare.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_ifThenElse.java" in {
        val files = filesForTest("J1_ifThenElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_if_then.java" in {
        val files = filesForTest("J1_if_then.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_multipleReturn.java" in {
        val files = filesForTest("J1_multipleReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_omittedvoidreturn.java" in {
        val files = filesForTest("J1_omittedvoidreturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_reachability_return" in {
        val files = filesForTest("J1_reachability_return") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_reachableIfBody.java" in {
        val files = filesForTest("J1_reachableIfBody.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_unreachableAutomation.java" in {
        val files = filesForTest("J1_unreachableAutomation.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_while1.java" in {
        val files = filesForTest("J1_while1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_while2.java" in {
        val files = filesForTest("J1_while2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "J1_whiletrue1.java" in {
        val files = filesForTest("J1_whiletrue1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not (throwA[Exception])
      }
      "Je_7_DefiniteAssignment_2LazyOr_Assignment.java" in {
        val files = filesForTest("Je_7_DefiniteAssignment_2LazyOr_Assignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_DefiniteAssignment_3LazyOr_Assignment.java" in {
        val files = filesForTest("Je_7_DefiniteAssignment_3LazyOr_Assignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterElseReturn.java" in {
        val files = filesForTest("Je_7_Reachability_AfterElseReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterIfReturn.java" in {
        val files = filesForTest("Je_7_Reachability_AfterIfReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterIfReturnElseReturn.java" in {
        val files = filesForTest("Je_7_Reachability_AfterIfReturnElseReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterReturnEmptyBlock.java" in {
        val files = filesForTest("Je_7_Reachability_AfterReturnEmptyBlock.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterReturn_Constructor.java" in {
        val files = filesForTest("Je_7_Reachability_AfterReturn_Constructor.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterValueReturn.java" in {
        val files = filesForTest("Je_7_Reachability_AfterValueReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_AfterVoidReturn.java" in {
        val files = filesForTest("Je_7_Reachability_AfterVoidReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_EmptyValueMethod.java" in {
        val files = filesForTest("Je_7_Reachability_EmptyValueMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_ForFalse_1.java" in {
        val files = filesForTest("Je_7_Reachability_ForFalse_1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_ForFalse_2.java" in {
        val files = filesForTest("Je_7_Reachability_ForFalse_2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_ReturnReturn.java" in {
        val files = filesForTest("Je_7_Reachability_ReturnReturn.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_WhileFalse_ConstantFolding.java" in {
        val files = filesForTest("Je_7_Reachability_WhileFalse_ConstantFolding.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_WhileFalse_Empty.java" in {
        val files = filesForTest("Je_7_Reachability_WhileFalse_Empty.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_WhileFalse_IfThenElse.java" in {
        val files = filesForTest("Je_7_Reachability_WhileFalse_IfThenElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_WhileTrue.java" in {
        val files = filesForTest("Je_7_Reachability_WhileTrue.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Reachability_WhileTrue_ConstantFolding.java" in {
        val files = filesForTest("Je_7_Reachability_WhileTrue_ConstantFolding.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Return_IfElseIf.java" in {
        val files = filesForTest("Je_7_Return_IfElseIf.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Return_IfIfNoElseElse.java" in {
        val files = filesForTest("Je_7_Return_IfIfNoElseElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Return_IfIfNot.java" in {
        val files = filesForTest("Je_7_Return_IfIfNot.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_7_Return_MissingInElse.java" in {
        val files = filesForTest("Je_7_Return_MissingInElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_ArrayAssign.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_ArrayAssign.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_ArrayIndexAssign.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_ArrayIndexAssign.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_ComplexInitializer.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_ComplexInitializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_FalseAndAssignment.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_FalseAndAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_FieldWithSameName.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_FieldWithSameName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_IfIfNot.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_IfIfNot.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_InitToItself.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_InitToItself.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_SomethingAndAssignment.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_SomethingAndAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_SomethingOrAssignment.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_SomethingOrAssignment.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_UninitializedExpInLvalue.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_UninitializedExpInLvalue.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_UninitializedInNewArray.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_UninitializedInNewArray.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_8_DefiniteAssignment_WhileFalse.java" in {
        val files = filesForTest("Je_8_DefiniteAssignment_WhileFalse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
      "Je_Widening.java" in {
        val files = filesForTest("Je_Widening.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must (throwA[Exception])
      }
    }
  }
}
