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
    }
  }
}
