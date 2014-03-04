package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import joosbox.parser._
import joosbox.lexer._
import java.io.File

class EnvironmentBuilderSpec extends Specification {
  "EnvironmentBuilder" should {
    val parser = Parser.Joos

    "Create environments from" in {

      "Test class" in {
        val input = """
public class Test {
  public Test() {
    if (true) {
      int x = 0;
    }
  }
  public void method() {
    int y = 0;
  }
}
        """
        val cu: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu))

        //  Verify that within the top-level scope, Test has some meaning.
        val fileScope: Environment = mapping.enclosingScopeOf(cu).get
        fileScope.lookup(NameLookup(InputString("Test", "Test.java", 0, 0))) must beEqualTo(cu.typeDeclaration)
      }

      "Test interface" in {
        val input = """
public interface Test {
  public void method();
}
        """
        val cu: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu))

        //  Verify that within the top-level scope, Test has some meaning.
        val fileScope: Environment = mapping.enclosingScopeOf(cu).get
        fileScope.lookup(NameLookup(InputString("Test", "Test.java", 0, 0))) must beEqualTo(cu.typeDeclaration)
      }

      "Test fully qualified lookup" in {
        val input = """
package joosbox.test;
public class Test {
  public Test() {
    if (true) {
      int x = 0;
    }
  }
  public void method() {
    int y = 0;
  }
}
        """
        val cu: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu))

        //  Verify that within the top-level scope, Test has some meaning.
        val fileScope: Environment = mapping.enclosingScopeOf(cu).get
        val qualifiedName = AbstractSyntaxNode.QualifiedName(Seq(
          InputString("joosbox", "Test.java", 0, 0),
          InputString("test", "Test.java", 0, 0),
          InputString("Test", "Test.java", 0, 0)
        ))
        fileScope.lookup(QualifiedNameLookup(qualifiedName)) must beEqualTo(cu.typeDeclaration)
      }

      "throw an error on qualified name clashes" in {
        val input1 = """
package joosbox.test;
public class Test { public Test() {} }
        """

        val input2 = """
package joosbox.test;
public class Test { public Test() {} }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1, cu2)) must throwA[SyntaxError]
      }

      "not fail on imports that do resolve correctly" in {
        val input1 = """
package joosbox.importable;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test;
import joosbox.importable.ImportedClass;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu1, cu2))

        //  Verify that within the top-level scope of input2, ImportedClass has some meaning.
        val file2Scope: Environment = mapping.enclosingScopeOf(cu2).get
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beEqualTo(cu1.typeDeclaration)
      }

      "fail on an import that does not exist" in {
        val input1 = """
package joosbox.importable;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test;
import joosbox.importable.ImportedClass2;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1, cu2)) must throwA[SyntaxError]
      }

      "verify the stdlib" in {
        def getAllFiles(base: File): Array[File] = {
          base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
        }

        def stdlibFilePaths: Seq[String] =
          getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

        val files = stdlibFilePaths
        val compilationUnits: Seq[AbstractSyntaxNode.CompilationUnit] = stdlibFilePaths.map(parser.parseFilename(_))

        EnvironmentBuilder.build(compilationUnits) must not(throwA[SyntaxError])
      }

      "correctly return a class from the same package" in {
        val input1 = """
package joosbox.test;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu1, cu2))

        //  Verify that within the top-level scope of input2, ImportedClass has some meaning.
        val file2Scope: Environment = mapping.enclosingScopeOf(cu2).get
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beEqualTo(cu1.typeDeclaration)
      }

      "fail to return a class from a different package" in {
        val input1 = """
package joosbox.tests;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu1, cu2))

        //  Verify that within the top-level scope of input2, ImportedClass has some meaning.
        val file2Scope: Environment = mapping.enclosingScopeOf(cu2).get
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beNone
      }

      "correctly return a class from a wildcard import" in {
        val input1 = """
package joosbox.test;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test2;
import joosbox.test.*;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu1, cu2))

        //  Verify that within the top-level scope of input2, ImportedClass has some meaning.
        val file2Scope: Environment = mapping.enclosingScopeOf(cu2).get
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beEqualTo(cu1.typeDeclaration)
      }

      "fail to return a class with a missing wildcard import" in {
        val input1 = """
package joosbox.test;
public class ImportedClass { public ImportedClass() {} }
        """

        val input2 = """
package joosbox.test2;
public class Test { public Test() { ImportedClass x = new ImportedClass(); } }
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "ImportedClass.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val cu2: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input2, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        val mapping: EnvironmentMapping = EnvironmentBuilder.build(Seq(cu1, cu2))

        //  Verify that within the top-level scope of input2, ImportedClass has some meaning.
        val file2Scope: Environment = mapping.enclosingScopeOf(cu2).get
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beNone
      }

      "fail to return a class with multiple conflicting field names" in {
        val input1 = """
package joosbox.test;
public class Test {
  public ImportedClass() {} 
  public int foo = 5;
  public int foo = 6;
}
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1)) must throwA[SyntaxError]
      }

      "fail to return a class that has two same-name local variables in same scope" in {
        val input1 = """
package joosbox.test;
public class Test {
  public Test() {
    int foo = 1;
    int foo = 3;
  } 
}
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1)) must throwA[SyntaxError]
      }

      "fail to return a class that has two same-name local variables in overlapping scope" in {
        val input1 = """
package joosbox.test;
public class Test {
  public Test() {
    int foo = 1;
    {
      int innerfoo = 2;
      int foo = 3;
    }
  } 
}
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1)) must throwA[SyntaxError]
      }

      "correctly build environments with properly nested variable redeclaration" in {
        val input1 = """
package joosbox.test;
public class Test {
  public Test() {}
  public int Method() {
    {
      int foo = 3;
    }
    int foo = 1;
    return foo;
  } 
}
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1)) must not(throwA[SyntaxError])
      }

      "fail to build with improperly nested variable redeclaration" in {
        val input1 = """
package joosbox.test;
public class Test {
  public Test() {
    int foo = 1;
    {
      int foo = 3;
    }
  } 
}
        """

        val cu1: AbstractSyntaxNode.CompilationUnit
          = parser.parseString(input1, "Test.java").asInstanceOf[AbstractSyntaxNode.CompilationUnit]

        EnvironmentBuilder.build(Seq(cu1)) must throwA[SyntaxError]
      }

    }
  }
}
