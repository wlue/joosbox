package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import joosbox.parser._
import joosbox.lexer._

class EnvironmentBuilderSpec extends Specification {
  "EnvironmentBuilder" should {
    val parser = Parser.Joos

    "Create an environment from" in {
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
        val fileScope: Environment = mapping.mapping(cu).asInstanceOf[Environment]
        fileScope.lookup(NameLookup(InputString("Test", "Test.java", 0, 0))) must beEqualTo(cu.classDeclaration)
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
        val fileScope: Environment = mapping.mapping(cu).asInstanceOf[Environment]
        fileScope.lookup(NameLookup(InputString("Test", "Test.java", 0, 0))) must beEqualTo(Some(cu.interfaceDeclarations(0)))
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
        val fileScope: Environment = mapping.mapping(cu).asInstanceOf[Environment]
        val qualifiedName = AbstractSyntaxNode.QualifiedName(Seq(
          InputString("joosbox", "Test.java", 0, 0),
          InputString("test", "Test.java", 0, 0),
          InputString("Test", "Test.java", 0, 0)
        ))
        fileScope.lookup(QualifiedNameLookup(qualifiedName)) must beEqualTo(cu.classDeclaration)
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
        val file2Scope: Environment = mapping.mapping(cu2).asInstanceOf[Environment]
        file2Scope.lookup(NameLookup(InputString("ImportedClass"))) must beEqualTo(cu1.classDeclaration)
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
    }
  }
}
