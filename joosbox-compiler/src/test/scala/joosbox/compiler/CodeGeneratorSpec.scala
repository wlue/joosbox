package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import java.io.File
import scala.sys.process.stringToProcess

class CodeGeneratorSpec extends Specification {
  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def linkAndTest(testFiles: Seq[String], expectedOutput: String, expectedReturnCode: Int) = {
    val prefix = if (System.getProperty("os.name") == "Mac OS X") {
      "mac"
    } else {
      "linux"
    }

    val preCommand = (prefix + "/" + "pre_scala_test.sh")
    val postCommand = (prefix + "/" + "post_scala_test.sh")

    stringToProcess(preCommand).!

    val files = testFiles ++ stdlibFilePaths
    CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])

    stringToProcess(postCommand).!



    // TODO: The following !! call throws an exception if the return code is not 0.
    // Until this is fixed, we're ignoring expected output if return code != 0.
    if (expectedReturnCode == 0) {
      stringToProcess("./main").! must beEqualTo(expectedReturnCode)
      stringToProcess("./main").!! must beEqualTo(expectedOutput)
    } else {
      stringToProcess("./main").! must beEqualTo(expectedReturnCode)
    }
  }

  "Compiler" should {
    "pass all custom marmoset tests" in {
      "class creation 1" in {
        linkAndTest(
          Seq("joosbox-compiler/src/test/resources/custom-tests/compiler_class_creation_1.java"),
          "", 42
        )
      }

      "variable access" in {
        linkAndTest(
          Seq("joosbox-compiler/src/test/resources/custom-tests/compiler_variable_access.java"),
          "", 11
        )
      }

      "while 1" in {
        linkAndTest(
          Seq("joosbox-compiler/src/test/resources/custom-tests/compiler_while_loop_1.java"),
          "", 1
        )
      }
    }

    "pass marmoset tests for assignment 5" in {
      "J1_1_Instanceof_OfAdditiveExpression.java" in {
        linkAndTest(
          Seq("joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_1_Instanceof_OfAdditiveExpression.java"),
          "", 123
        )
      }
    }
  }
}