package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import joosbox.parser._
import joosbox.lexer._

class TypeLinkerSpec extends Specification {
  "TypeLinker" should {
    val parser = Parser.Joos

    val testFile = """
package joosbox.test;

public class Test {
  public Apple() {}
}
    """
    val appleFile = """
package joosbox.test;

public class Apple {
  public Apple() {}
}
    """

    val precompiledNodes = Seq(
      parser.parseString(testFile, "prebuilt/Test.java"),
      parser.parseString(appleFile, "prebuilt/Apple.java")
    ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]

    "check for imports" in {
      "no imports for class" in {
        val input = """
public class Test {
  public Test() {}
}
        """
        val nodes = Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "no collisions for class" in {
        val input = """
import joosbox.test.Apple;

public class Test {
  public Test() {}
}
        """
        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "no collisions for interface" in {
        val input = """
import joosbox.test.Apple;

public class Interface {
  public Test() {}
}
        """
        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Interface.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "no collisions for interfaces" in {
        val input = """
import joosbox.test.Apple;

public class SomethingElse {
  public Test() {}
}
        """
        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "SomethingElse.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "no imports for interface" in {
        val input = """
public interface Test {
}
        """
        val nodes = Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "colliding with class name" in {
        val input = """
import joosbox.test.Test;

public class Test {
  public Test() {}
}
        """

        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "colliding with interface name" in {
        val input = """
import joosbox.test.Test;

public interface Test {
}
        """

        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }
    }
  }
}
