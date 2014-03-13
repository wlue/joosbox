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
         // TODO: This is commented out because it doesn't work yet. AmbiguousName resolution is required.
         /*
        val argTypes:Seq[Type] = s.args.flatMap {
          case x: StringLiteral => {
            Some(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_)))))
          }
          case c: CastExpression => Some(c.targetType)
        }.collect{case t: Type => t}

        s.name match {
          case sn: SimpleName => {
            val methodLookup = MethodLookup(sn.value, argTypes)
            println("Perform methodLookup: " + methodLookup)
            mapping.enclosingScopeOf(s).get.lookup(methodLookup) match {
              case None => throw new SyntaxError("Unknown method " + methodLookup)
              case _ => Unit
            }
          }
          case qn: QualifiedName => {
            //  Disambiguate callee:
            val callee = qn.value.dropRight(1)
            val methodLookup = MethodLookup(qn.value.last, argTypes)
            println("Perform methodLookup: " + methodLookup + " in other scope: " + callee)

            disambiguateReferenceToMethodCall(QualifiedName(callee))(mapping, s) match {
              case None => throw new SyntaxError("Unknown object for method invocation: " + s.name.niceName)
              case Some(cd: ClassDeclaration) => {
                val declarationEnvironment: Environment = mapping.enclosingScopeOf(cd.body).get

                println("Found ClassDeclaration from accessor: " + callee + "\n\n")

                declarationEnvironment.lookup(methodLookup) match {
                  case Some(a: AbstractSyntaxNode) => {
                    println("Found somtehing! " + a)
                  }
                  case None => throw new SyntaxError("Unknown method: " + methodLookup + " in env: " + declarationEnvironment)
                }
              }

              case Some(asn: AbstractSyntaxNode) => {
                //  TODO: To handle this, we need to do type checking to see
                //  what type this AST node is first.
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

//              println(x + " with remainder " + rem + "\n")
            } else {
//              println(x)
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


  //TODO THIS IS BROKEN
  //TODO THIS SHOULD RETURN THE AST NODE 
  def disambiguateReferenceToMethodCall(name: QualifiedName)(
    implicit mapping: EnvironmentMapping,
    owner: AbstractSyntaxNode
  ): Option[Referenceable] = {
    //  Given a name of the form:
    //    X.Y.Z
    //  Try the following in order:
    //    IdentifierLookup(X)
    //    NameLookup(X)
    //    QualifiedNameLookup(X)
    //    QualifiedNameLookup(X.Y)
    //    QualifiedNameLookup(X.Y.Z)
    //  ...until a Type is returned. Once a type comes back,
    //  look for the remainder in the scope of the returned.
    //  If there is any part of the name left unmatched, return that.

//    println("\ndisambiguateReferenceToMethodCall(" + name + ")\n")
    val environment = mapping.enclosingScopeOf(owner).get
    if (name.value.size == 1) {
      Seq(
        IdentifierLookup(name.value(0)),
        NameLookup(name.value(0)),
        QualifiedNameLookup(name)
      ).toStream.flatMap(environment.lookup(_)).headOption
    } else if (name.value.size > 1) {
//      println("\n\nQuerying for prefixes: " + name.prefixesIncludingSelf + "\n\n")
      name.prefixesIncludingSelf.toStream.flatMap{
        x: QualifiedName => environment.lookup(QualifiedNameLookup(x))
      }.headOption match {
        case Some(r: Referenceable) => Some(r)
        case None => {
          val lookup:Seq[InputString] = name.value.slice(0, 1)
          
          disambiguateReferenceToMethodCall(QualifiedName(lookup)) match {
            case Some(scopeOwner: Referenceable) => {
              //  If the top level (i.e.: "X") is an object or class or whatnot,
              //  then look up "Y.Z" within the scope of that object.
//              println("Found scope of " + lookup)
              val r = disambiguateReferenceToMethodCall(QualifiedName(name.value.drop(1)))(mapping, scopeOwner)
//              println("Found AST node: " + r)
              r
            }
            case None => {
              throw new SyntaxError("Could not find object or class " + lookup(0))
            }
          }

        }
      }
    } else {
      None
    }
  }
}
