package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable

/**
   - No single-type-import declaration clashes with the class or interface
     declared in the same file.

   - No two single-type-import declarations clash with each other.

   - All type names must resolve to some class or interface declared in some
     file listed on the Joos command line.

   - All simple type names must resolve to a unique class or interface.

   - When a fully qualified name resolves to a type, no strict prefix of the
     fully qualified name can resolve to a type in the same environment.

   - No package names or prefixes of package names of declared packages,
     single-type-import declarations or import-on-demand declarations that are
     used may resolve to types, except for types in the default package.

   - Every import-on-demand declaration must refer to a package declared in some
     file listed on the Joos command line. That is, the import-on-demand
     declaration must refer to a package whose name appears as the package
     declaration in some source file, or whose name is a prefix of the name
     appearing in some package declaration.
*/

object TypeLinker {
  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      TypeLinker.check(unit)(mapping)
    }
    Map.empty
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    import AbstractSyntaxNode.{
      ImportDeclaration,
      SingleTypeImportDeclaration,
      TypeImportOnDemandDeclaration,
      InterfaceDeclaration,
      ClassDeclaration,
      QualifiedName
    }

    node match {
      case node: CompilationUnit =>
        val imports: Seq[ImportDeclaration] = node.importDeclarations
        val interfaces: Seq[InterfaceDeclaration] = node.interfaceDeclarations
        val klass: Option[ClassDeclaration] = node.classDeclaration

        klass match {
          case Some(ClassDeclaration(className, _, _, _, _)) =>
            imports.foreach {
              case SingleTypeImportDeclaration(QualifiedName(nameSeq)) =>
                val packageName: InputString = nameSeq.last
                if (className.value == packageName.value) {
                  throw new SyntaxError("Package import cannot be the same name as class name.");
                }

              case _ => Unit
            }
          case None => Unit
        }

      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }
}