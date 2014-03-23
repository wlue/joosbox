package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import java.io.File
import scala.sys.process.Process

class CodeGeneratorSpec extends Specification {

  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a5/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  def filesForCustomTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/custom-tests/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  /*
    Run one code generation test on Mac OS X.
   */
  def runTest(name: String, isCustom: Boolean = false): Unit = {
    //  Delete "main" output file.
    val main = new File("main")
    if (main.isFile) {
      main.delete()
    }

    //  Delete all files in ./output first.
    val outputDir: File = new File("output")
    if (outputDir.isFile) {
      throw new Exception("'output' exists and is not a directory")
    } else if (!outputDir.exists()) {
      outputDir.mkdirs()
    }
    outputDir.listFiles().foreach(_.delete)

    var files: Seq[String] = Seq.empty[String]
    if (isCustom) {
      files = filesForCustomTest(name) ++ stdlibFilePaths
    } else {
      files = filesForTest(name) ++ stdlibFilePaths
    }
    CompilerRunner.runTestable(files.toArray)

    outputDir.listFiles().foreach(f => {
      Process("nasm -O1 -f elf -g -F dwarf -o output/" + f.getName.replace(".s", ".o") + " output/" + f.getName).!
    })
    Process("nasm -O1 -f elf -g -F dwarf -o output/runtime.o joosbox-compiler/src/test/resources/stdlib/runtime.s").!
    Process("ld -m elf_i386 -o main " + outputDir.listFiles().filter(_.getName.endsWith(".o")).map(f => "output/" + f.getName).mkString(" ")).!
    Process("main").!
  }

  "Compiler" should {
    "pass basic tests" in {
      "compiler_return_1" in {
        runTest("compiler_return_1.java", true)
        true
      }
    }
    "pass marmoset tests for assignment 5" in {
      true
    }
  }
}
