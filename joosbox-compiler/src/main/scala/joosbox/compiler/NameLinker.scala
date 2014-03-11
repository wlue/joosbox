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
                throw new SyntaxError("Unknown name " + s + ", enclosing scope: " + mapping.enclosingScopeOf(name))
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
          case None => throw new SyntaxError("Unknown name " + q + ", enclosing scope: " + mapping.enclosingScopeOf(name))
          case Some((x: Referenceable, rem: Seq[InputString])) => {
            //println("Name " + q + " resolved to " + x + " with remainder " + rem)
            Unit
          }
        }
      }
      case _ => {}
    })
  }
}
