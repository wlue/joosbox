package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import joosbox.parser._

import AbstractSyntaxNode._

object NameLinker {
  def link(units: Seq[CompilationUnit]): Map[Any, Referenceable] = {
    units.foreach { unit => NameLinker.check(unit) }
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
                    val name = PackageName(identifier, Some(pn))
                    name.scope = a.scope
                    name
                  }
                  case Some(scopes: Seq[ScopeEnvironment]) => {
                    val resolvedType = scopes.flatMap(
                      _.lookup(EnvironmentLookup.lookupFromName(TypeName(identifier)))
                    ).headOption

                    resolvedType match {
                      case Some(_) => {
                        val name = TypeName(identifier, Some(pn))
                        name.scope = a.scope
                        name
                      }

                      //  This PackageName may not exist, but the docs suggest that
                      //  this will be checked at a later stage. This might need checking
                      //  now.
                      case None => {
                        val name = PackageName(identifier, Some(pn))
                        name.scope = a.scope
                        name
                      }
                    }
                  }
                }
              }
              case tn: TypeName => {
                val name = ExpressionName(identifier, Some(tn))
                name.scope = a.scope
                name
              }
              case en: ExpressionName => {
                val name = ExpressionName(identifier, Some(en))
                name.scope = a.scope
                name
              }
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
            val expressionLookup = EnvironmentLookup.lookupFromName(ExpressionName(identifier))
            env.lookup(expressionLookup) match {
              case Some(x) => {
                val name = ExpressionName(identifier)
                name.scope = a.scope
                name
              }
              case None => {
                env.lookup(EnvironmentLookup.lookupFromName(TypeName(identifier))) match {
                  //  To ensure that this name is not semantically ambiguous
                  //  in the local scope of a class or interface, look up the
                  //  name again from the point of view of Class or Interface
                  //  body, which has all fields in scope.
                  //  If there is a hit for the lookup, then throw an error,
                  //  as this name is ambiguous.
                  case Some(c: ClassDeclaration) => {
                    if (c.body.scope.get.lookup(expressionLookup) != None) {
                      throw new SyntaxError("Name is semantically ambiguous: " + a)
                    }
                    val name = TypeName(identifier)
                    name.scope = a.scope
                    name
                  }
                  case Some(i: InterfaceDeclaration) => {
                    if (i.body.scope.get.lookup(expressionLookup) != None) {
                      throw new SyntaxError("Name is semantically ambiguous: " + a)
                    }
                    val name = TypeName(identifier)
                    name.scope = a.scope
                    name
                  }
                  case None => {
                    env.packageScope(PackageNameLookup(PackageName(identifier))) match {
                      case Some(_) => {
                        val name = PackageName(identifier)
                        name.scope = a.scope
                        name
                      }
                      case None => throw new SyntaxError("Unknown name " + identifier)
                    }
                  }
                  case _ =>
                    throw new SyntaxError("Type name lookup resolved to non-type: " + identifier)
                }
              }
            }
          }

        }
        case n: Name => n
      }
    } else {
      name match {
        case m: MethodName => m // TODO: disambiguate method name parameters first for lookup here.
        case _ => {
          env.lookup(EnvironmentLookup.lookupFromName(name)) match {
            case Some(x) => name
            case None => throw new SyntaxError("Unknown name reference: " + name)
          }
        }
      }
    }
  }

  def check(node: AbstractSyntaxNode) {
    node match {
      //  TODO: Really nasty edge case for J1_fieldinit2.java
      //  If we're in a FieldDeclaration and we assign to a name,
      //  and that name does not exist (yet), we need to allow the
      //  assignment anyways by looking up the referenced field anyways.
      //  This should be done by picking out the ClassDeclaration, finding
      //  the scope of the last ClassMemberDeclaration, and looking up
      //  the assigned-to name in that scope.

      case m: MethodName => Unit
      case p: PackageName => Unit
      case a: AmbiguousName => Unit

      case t: ThisKeyword =>
        TypeChecker.resolveType(t) match {
        case Some(_) => Unit
        case None => throw new SyntaxError("Could not resolve 'this' keyword: " + t + " (is it called from a static context?)")
      }

      case p: Name => TypeChecker.resolveType(p) match {
        case Some(_) => Unit
        case None => throw new SyntaxError("Could not resolve name: " + p)
      }
      case s: SimpleMethodInvocation => TypeChecker.resolveType(s) match {
        case Some(_) => Unit
        case None => throw new SyntaxError("Could not resolve method: " + s)
      }
      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }

  def findMethodScope(name: Name, env: Environment): Option[Environment] = {
    name match {
      case MethodName(_, Some(prefix: Name)) => findMethodScope(prefix, env)
      case MethodName(_, None) => Some(env)

      case TypeName(_, Some(prefix: Name)) => findMethodScope(prefix, env)
      case PackageName(_, Some(prefix: Name)) => findMethodScope(prefix, env)
      case ExpressionName(_, Some(prefix: Name)) => findMethodScope(prefix, env)

      case n: Name => env.lookup(EnvironmentLookup.lookupFromName(n)).get.scope
    }
  }
}
