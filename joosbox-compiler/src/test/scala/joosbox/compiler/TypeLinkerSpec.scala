package joosbox.compiler.test

import org.specs2.mutable._
import java.io.File

import joosbox.compiler._
import joosbox.parser._
import joosbox.lexer._

class TypeLinkerSpec extends Specification {
  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  val parser = Parser.Joos

  val javaLangObject = """
package java.lang;
public class Object {
  public Object() {}
}
  """

  val stdlibNodes: Seq[AbstractSyntaxNode.CompilationUnit] = Seq(
    parser.parseString(javaLangObject, "java/lang/Object.java")
  ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]

  "TypeLinker" should {
    "check for imports" in {
      val testFile = """
  package joosbox.test;

  public class Test {
    public Test() {}
  }
      """

      val testFileSomethingElse = """
  package joosbox.something_else;

  public class Test {
    public Test() {}
  }
      """

      val appleFile = """
  package joosbox.test;

  public class Apple {
    public Apple() {}
  }
      """

      val precompiledNodes = stdlibNodes ++ Seq(
        parser.parseString(testFile, "joosbox/test/Test.java"),
        parser.parseString(testFileSomethingElse, "joosbos/something_else/Test.java"),
        parser.parseString(appleFile, "joosbox/test/Apple.java")
      ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]

      "importing nonexistant class" in {
        val input = """
import foo.bar.Baz;

public class Main {
  public Main() {}
}
        """
        val nodes = Seq(parser.parseString(input, "Main.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        TypeLinker.link(nodes, EnvironmentBuilder.build(nodes)) must throwA[Exception]
      }

      "importing nonexistant package" in {
        val input = """
import foo.bar.*;

public class Main {
  public Main() {}
}
        """
        val nodes = Seq(parser.parseString(input, "Main.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        TypeLinker.link(nodes, EnvironmentBuilder.build(nodes)) must throwA[Exception]
      }

      "no imports for class" in {
        val input = """
public class Test {
  public Test() {}
}
        """
        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Test.java")
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

      "no imports for interface" in {
        val input = """
public interface Test {
}
        """
        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "Test.java")
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

      "colliding with single package import" in {
        val input = """
import joosbox.test.Apple;
import joosbox.test.Test;
import joosbox.something_else.Test;

public class NotTest {
  public NotTest() {}
}
        """

        val nodes = precompiledNodes ++ Seq(parser.parseString(input, "NotTest.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }
    }

    "All type names must resolve to some class or interface declared in some file listed on the Joos command line." in {
      "self referential class reference" in {
        val input = """
public class Test {
  public Test() {
    Test test = null;
  }
}
        """

        val nodes = stdlibNodes ++ Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }

      "invalid class or interface type reference" in {
        val input = """
public class Test {
  public Test() {
    NotTest notTest = null;
  }
}
        """

        val nodes = stdlibNodes ++ Seq(parser.parseString(input, "Test.java")
          .asInstanceOf[AbstractSyntaxNode.CompilationUnit])
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "valid package type class reference" in {
        val other = """
package joosbox.test;

public class Other {
  public Other() {}
}
        """

        val input = """
import joosbox.test.Other;

public class Test {
  public Test() {
    Other other = null;
  }
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(other, "joosbox/test/Other.java"),
          parser.parseString(input, "Test.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must not(throwA[Exception])
      }
    }

    "prefixes for imports don't collide" in {
      "prefix collision" in {
        val three = """
package One.Two;

public class Three {
  public Three() {}
}
        """

        val two = """
package One;

public class Two {
  public Two() {}
}
        """

        val input = """
import One.Two;

public class Main {
  public Main() {
    One.Two.Three three = null;
  }
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(three, "One/Two/Three.java"),
          parser.parseString(two, "One/Two.java"),
          parser.parseString(input, "Main.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "same package as class name" in {
        val input = """
package Main;

public class Main {
  public Main() {
    new Main.Main();
  }
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(input, "Main.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "package resolves to a class or interface" in {
        val input = """
package foo;

public class bar {
  public bar() {}
}
        """

        val input2 = """
package foo.bar;

public class baz {
  public baz() {}
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(input, "foo/bar.java"),
          parser.parseString(input2, "foo/bar/baz.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "package resolves to a class or interface" in {
        val input = """
package foo;

public class bar {
  public bar() {}
}
        """

        val input2 = """
package foo.bar;

public class baz {
  public baz() {}
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(input, "foo/bar.java"),
          parser.parseString(input2, "foo/bar/baz.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

      "conflict with wildcard" in {
        val al = """
package java.util;

public class ArrayList {
  public ArrayList() {}
}
        """

        val al2 = """
package java.util.ArrayList.foo;

public class bar {
  public bar() {}
}
        """
        val input = """
public class Main {
  public Main() {}
}
        """

        val nodes = stdlibNodes ++ Seq(
          parser.parseString(al, "java/util/ArrayList.java"),
          parser.parseString(al2, "java/util/ArrayList/foo/bar.java"),
          parser.parseString(input, "Main.java")
        ).asInstanceOf[Seq[AbstractSyntaxNode.CompilationUnit]]
        val mapping = EnvironmentBuilder.build(nodes)
        TypeLinker.link(nodes, mapping) must throwA[Exception]
      }

    }
  }
}
