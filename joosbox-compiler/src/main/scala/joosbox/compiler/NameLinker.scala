package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode._

object NameLinker {
  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      NameLinker.check(unit)(mapping)
    }
    Map.empty
  }

  def disambiguateName(name: Name)(implicit env: Environment): Name = {
    if (name.isAmbiguous) {
      name match {
        case e: ExpressionName => ExpressionName(e.value, Some(disambiguateName(e.prefix.get)))
        case m: MethodName => MethodName(m.value, Some(disambiguateName(m.prefix.get)))
        case a: AmbiguousName => a.prefix match {
          case Some(an: AmbiguousName) => {
            val identifier = a.value

            disambiguateName(an) match {
              case pn: PackageName => {
                /*
                If there is a package whose name is the name to the left of the 
                "." and that package contains a declaration of a type whose name 
                is the same as the Identifier, then this AmbiguousName is 
                reclassified as a TypeName.
                */
                env.packageScope(PackageNameLookup(pn)) match {
                  case Some(Seq()) => {
                    //  This package is a prefix of another, but not actually a package itself.
                    //  As per the docs:

                    /*
                    This AmbiguousName is reclassified as a PackageName.
                    A later step determines whether or not a package of that name 
                    actually exists.
                    */
                    PackageName(identifier, Some(pn))
                  }
                  case Some(scopes: Seq[ScopeEnvironment]) => {
                    val resolvedType = scopes.flatMap(
                      _.lookup(EnvironmentLookup.lookupFromName(TypeName(identifier)))
                    ).headOption

                    resolvedType match {
                      case Some(_) => TypeName(identifier, Some(pn))

                      //  This PackageName may not exist, but the docs suggest that
                      //  this will be checked at a later stage. This might need checking
                      //  now.
                      case None => PackageName(identifier, Some(pn))
                    }
                  }
                }
              }
              case tn: TypeName => ExpressionName(identifier, Some(tn))
              case en: ExpressionName => ExpressionName(identifier, Some(en))
              case _ => throw new SyntaxError(
                "Name disambiguation of '" + an.niceName + "' returned unexpected results."
              )
            }
          }

          //  If we have a prefix and it's not ambiguous, then blow up - that doesn't make sense.
          case Some(n: Name) => throw new SyntaxError("AmbiguousName has non-ambiguous prefix.")

          // If the AmbiguousName is a simple name, consisting of a single Identifier:
          case None => {
            val identifier = a.value

            env.lookup(EnvironmentLookup.lookupFromName(ExpressionName(identifier))) match {
              case Some(x) => ExpressionName(identifier)
              case None => {
                env.lookup(EnvironmentLookup.lookupFromName(TypeName(identifier))) match {
                  case Some(x) => TypeName(identifier)
                  case None => {
                    env.packageScope(PackageNameLookup(PackageName(identifier))) match {
                      case Some(_) => PackageName(identifier)
                      case None => throw new SyntaxError("Unknown name " + identifier)
                    }
                  }
                }
              }
            }
          }

        }
        case n: Name => n
      }
    } else {
      name
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
      case p: Name => disambiguateName(p)(mapping.enclosingScopeOf(p).get)
      case s: SimpleMethodInvocation => {
        val argTypes:Seq[Type] = s.args.flatMap {
          case x: StringLiteral => {
            Some(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName))
          }
          case c: CastExpression => Some(c.targetType)
          case _ => None
        }.collect{case t: Type => t}
        //println("Simple method invocation: " + s + "\nhas args: " + argTypes)
      }

      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }

  def verifyNameExists(name: Name)(implicit mapping: EnvironmentMapping) {
    if (name.isAmbiguous) {
      //  println("Name is ambiguous, we need to resolve this first.")
    } else {
      mapping.enclosingScopeOf(name).get.lookup(EnvironmentLookup.lookupFromName(name)) match {
        case None => throw new SyntaxError("Unknown name " + name)
        case _ => Unit
      }
    }
  }
}
