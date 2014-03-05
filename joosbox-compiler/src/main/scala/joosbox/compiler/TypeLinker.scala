package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode._

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
    node match {
      case node: CompilationUnit => {
        val classPackage: Option[PackageDeclaration] = node.packageDeclaration
        val imports: Seq[ImportDeclaration] = node.importDeclarations
        val typeDeclaration: Option[TypeDeclaration] = node.typeDeclaration

        // Check for single type import declaration clashes between each other.
        imports.foreach { singleImport =>
          imports.foreach { otherImport =>
            (singleImport, otherImport) match {
              case (
                SingleTypeImportDeclaration(firstName),
                SingleTypeImportDeclaration(secondName)
              ) => {
                var Seq(firstNameSeq: Seq[String], secondNameSeq: Seq[String]): Seq[Seq[String]] = Seq(firstName, secondName).map {
                  case SimpleName(name) => Seq(name.value)
                  case QualifiedName(names) => names.map { input: InputString => input.value }
                }

                if (firstNameSeq.size == 1 || firstNameSeq != secondNameSeq) {
                  if (firstNameSeq.last == secondNameSeq.last) {
                    throw new SyntaxError("Import " + firstName + " conflicts with " + secondName + ".")
                  }
                }
              }
              case _ => {
              }
            }
          }
        }

        typeDeclaration.foreach { typeDeclaration =>
          var className = typeDeclaration match {
            case ClassDeclaration(name, _, _, _, _) => name
            case InterfaceDeclaration(name, _, _, _) => name
          }

          imports.foreach {
            case SingleTypeImportDeclaration(name) =>
              val importName: Seq[InputString] = name match {
                case QualifiedName(values) => values
                case SimpleName(value) => Seq(value)
              }

              if (classPackage.isEmpty) {
                // In the default package, all class clashes are invalid
                if (className.value == importName.last.value) {
                  throw new SyntaxError("Package import cannot be the same name as class name.")
                }
              } else {
                val classPackageName: Seq[InputString] = classPackage.get.name match {
                  case QualifiedName(values) => values ++ Seq(className)
                  case SimpleName(value) => Seq(value) ++ Seq(className)
                }

                // A class may import itself, but no other clashing classes
                if (classPackageName != importName) {
                  if (classPackageName.last.value == importName.last.value) {
                    throw new SyntaxError("Package import cannot be the same name as class name.")
                  }
                }
              }
            case _ => {
            }
          }
        }
      }
      case ref: ReferenceType => {
        val nameOption: Option[Name] = ref match {
          case ClassOrInterfaceType(name) => Some(name)
          case ClassType(name) => Some(name)
          case InterfaceType(name) => Some(name)
          case _ => None
        }

        for {
          name <- nameOption
          environment <- mapping.enclosingScopeOf(ref)
        } {
          var (lookup: EnvironmentLookup, niceName: String) = name match {
            case SimpleName(input) => (NameLookup(input), input.value)
            case name: QualifiedName => (QualifiedNameLookup(name), name.value.map(_.value).mkString("."))
          }

          environment.lookup(lookup) match {
            case None =>
              throw new SyntaxError("Could not look up type " + niceName + ".")
            case _ => {}
          }
        }
      }
      case _ => {}
    }

    node.children.foreach { node => check(node) }
  }
}
