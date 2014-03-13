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

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
      case ct: ClassType => verifyNamesExist(Seq(ct.name))(mapping)
      case e: ConditionalExpression => verifyNamesExist(e.children.collect {case c: Name => c})(mapping)
      case e: ArithmeticExpression => verifyNamesExist(e.children.collect {case c: Name => c})(mapping)
      case e: RelationalExpression => verifyNamesExist(e.children.collect {case c: Name => c})(mapping)
      case s: SimpleMethodInvocation => {
        /*
         * Split the method's name into parts:
         *          String.valueOf
         *      |------^      ^------|
         *    Class              MethodName
         * Look up the class by its qualified name, if there is one.
         * Then, look up the method in that class's scope.
         * If there is no qualified name, look up the method in the current scope.
         */

         // TODO: This is commented out because it doesn't work yet. AmbiguousName resolution is required.
         /*
        s.name match {
          case SimpleName(v: InputString) => {
            mapping.enclosingScopeOf(s).get.lookupMethodsByName(v) match {
              case Seq(method: MethodDeclaration) => {
                //println("Method name lookup " + v + " found exactly one method:\n" + method)
              }
              case Seq(method: MethodDeclaration, others@_*) => {
                //println("Found " + (others.size + 1) + " methods named " + v)
              }
              case Seq() => throw new SyntaxError("Unknown method: " + v)
            }
          }

          case QualifiedName(components: Seq[InputString]) => {
            val accessor:QualifiedName = QualifiedName(components.dropRight(1))

            disambiguateReferenceToMethodCall(accessor)(mapping.enclosingScopeOf(s).get) match {
              case None => throw new SyntaxError("Unknown object for method invocation: " + s.name.niceName)
              case Some(cd: ClassDeclaration) => {
                val declarationEnvironment: Environment = mapping.enclosingScopeOf(cd.body).get
                val v: InputString = components.last

                declarationEnvironment.lookupMethodsByName(v) match {
                  case Seq(method: MethodDeclaration) => {
                    //println("Method name lookup " + v + " found exactly one method:\n" + method)
                  }
                  case Seq(method: MethodDeclaration, others@_*) => {
                    //println("Found " + (others.size + 1) + " methods named " + v)
                  }
                  case Seq() => throw new SyntaxError("Unknown method: " + v + " in env: " + declarationEnvironment)
                }
              }

              case Some(asn: AbstractSyntaxNode) => {
                //println("Method call on unhandled AST node: " + asn)
              }
            }
          }
        }
        */
        verifyNamesExist(s.args.collect {case c: Name => c})(mapping)
      }

      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }

  def verifyNamesExist(names: Seq[Name])(implicit mapping: EnvironmentMapping) {
    names.foreach(name => name match {
      case s: SimpleName => {
        mapping.enclosingScopeOf(name).get.lookup(IdentifierLookup(s.value)) match {
          case None => {
            mapping.enclosingScopeOf(name).get.lookup(NameLookup(s.value)) match {
              case None => 
                throw new SyntaxError("Unknown name " + s)
              case _ => Unit
            }
          }
          case _ => Unit
        }
      }
      case q: QualifiedName => {
        //  Lookup the entire qualified name, but fall back to prefixes if necessary.
        //  If a prefix matches a variable or a field, then the remainder refers to
        //  non-static fields or methods on that object. (Currently, for a3, those are not resolved.)
        val results: Seq[(Referenceable, Seq[InputString])] = q.prefixesIncludingSelf.flatMap{prefix: QualifiedName => {
          if (prefix.value.size == 1) {
            mapping.enclosingScopeOf(name).get.lookup(IdentifierLookup(prefix.value(0))) match {
              case None => {
                val result = mapping.enclosingScopeOf(name).get.lookup(NameLookup(prefix.value(0)))
                result match {
                  case Some(x: Referenceable) => Some((x, q.value.drop(prefix.value.size)))
                  case None => None
                }
              }
              case Some(x: Referenceable) => {
                Some((x, q.value.drop(prefix.value.size)))
              }
            }
          } else if (prefix.value.size > 1) {
            val result = mapping.enclosingScopeOf(name).get.lookup(QualifiedNameLookup(prefix))
            result match {
              case Some(x: Referenceable) => Some((x, q.value.drop(prefix.value.size)))
              case None => None
            }
          } else {
            None
          }
        }}

        results.headOption match {
          case None => throw new SyntaxError("Unknown name " + q)
          case Some((x: Referenceable, rem: Seq[InputString])) => {
            if (rem.size > 0) {
              //  Verify that the Referenceable resolves to a class,
              //  and verify that the class has a static field or method
              //  with this name on it.

              //println(x + " with remainder " + rem + "\n")
            }
            Unit
          }
        }
      }
      case _ => {}
    })
  }

  def findByStringInEnvironment(name: InputString)(implicit environment: Environment): Option[Referenceable] = {
    val queries = Seq(IdentifierLookup(name), NameLookup(name))
    queries.toStream.flatMap(environment.lookup(_)).headOption
  }

  def disambiguateReferenceToMethodCall(name: QualifiedName)(implicit environment: Environment): Option[Referenceable] = {
    if (name.value.size == 1) {
      Seq(
        IdentifierLookup(name.value(0)),
        NameLookup(name.value(0)),
        QualifiedNameLookup(name)
      ).toStream.flatMap(environment.lookup(_)).headOption
    } else if (name.value.size > 1) {
      Seq(QualifiedNameLookup(name)).toStream.flatMap(environment.lookup(_)).headOption
    } else {
      None
    }
  }
}
