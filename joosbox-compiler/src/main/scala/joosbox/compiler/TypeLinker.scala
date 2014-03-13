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
      case pkg: PackageDeclaration => {
        // Make sure the package does not resolve to a type.

        // Lookup the top level environment, and make sure the package and any 
        // of its prefixes does not resolve to a type.
        val environment = mapping.environment
        val qualifiedName: QualifiedName = pkg.name.toQualifiedName

        qualifiedName.prefixesIncludingSelf.foreach { prefix =>
          var lookupOption: Option[EnvironmentLookup] = prefix match {
            case QualifiedName(Seq()) => None
            case QualifiedName(Seq(name)) => Some(NameLookup(name))
            case _ => Some(QualifiedNameLookup(prefix))
          }

          for {
            lookup <- lookupOption
            result <- environment.lookup(lookup)
          } {
            throw new SyntaxError("Package name " + pkg.name.niceName + " resolves to a type " + result +  ".")
          }
        }
      }
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
                  case name: TypeName => name.toSeq.map(_.value)
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
          var className: TypeName = typeDeclaration match {
            case ClassDeclaration(name, _, _, _, _) => name
            case InterfaceDeclaration(name, _, _, _) => name
          }

          imports.foreach {
            case SingleTypeImportDeclaration(name) =>
              val importName: Seq[InputString] = name.toSeq

              if (classPackage.isEmpty) {
                // In the default package, all class clashes are invalid
                if (className.value == importName.last.value) {
                  throw new SyntaxError("Package import cannot be the same name as class name.")
                }
              } else {
                val classPackageName: Seq[InputString] = classPackage.get.name.toQualifiedName.value ++ className.toSeq

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
          var lookup: EnvironmentLookup = EnvironmentLookup.lookupFromName(name)
          environment.lookup(lookup) match {
            case None => throw new SyntaxError("Could not look up type " + lookup + ". " + name)
            case _ => {
              name match {
                // If a qualified name was found, then make sure no strict prefix resolves.
                case qualifiedName: QualifiedName => {
                  qualifiedName.prefixes.foreach { prefix =>
                    var lookupOption: Option[EnvironmentLookup] = prefix match {
                      case QualifiedName(Seq()) => None
                      case QualifiedName(Seq(name)) => Some(NameLookup(name))
                      case _ => Some(QualifiedNameLookup(prefix))
                    }

                    for {
                      lookup <- lookupOption
                      result <- environment.lookup(lookup)
                    } {
                      throw new SyntaxError("Type " + name.niceName + " conflicts with " + prefix.niceName + ".")
                    }
                  }
                }
                case _ => {}
              }
            }
          }
        }
      }

      case _ => {}
    }

    node.children.foreach { node => check(node) }
  }
}
