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
      case p: PackageName => verifyNameExists(p)(mapping)
      case t: TypeName => verifyNameExists(t)(mapping)
      case e: ExpressionName => verifyNameExists(e)(mapping)
      case m: MethodName => {
        //println("Got method name " + m + " that needs args to link.") 
        //verifyNamesExist(Seq(p.name))(mapping)
      }
      case a: AmbiguousName => {
        //println("Got ambiguous name that needs disambiguating: " + a + "\n")
      }
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
