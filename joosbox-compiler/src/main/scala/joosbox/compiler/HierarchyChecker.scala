package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable

/**
  - The hierarchy must be acyclic.

  -* A class must not extend an interface.

  -* A class must not extend a final class.

  -* A class must not implement a class.

  -* An interface must not extend a class.

  -* An interface must not be repeated in an implements clause of an class.

  -* An interface must not be repeated in an extends clause of an interface.

  - A class or interface must not declare two methods with the same signature.

  - A class must not declare two constructors with the same parameter type.

  - A class or interface must not contain (declare or inherit) two methods with
    the same signature but different return types

  - A class that contains (declares or inherits) any abstract methods must be
    abstract.

  - A nonstatic method must not replace a static method.

  - A method must not replace a method with a different return type.

  - A protected method must not replace a public method.

  - A method must not replace a final method.
*/

object HierarchyChecker {
  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      HierarchyChecker.check(unit)(mapping)
    }
    Map.empty
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    import AbstractSyntaxNode.{
      InterfaceDeclaration,
      ClassDeclaration,
      InterfaceType,
      ClassType,
      Modifier,
      SimpleName,
      QualifiedName,
      FinalKeyword
    }

    node match {
      case node: ClassDeclaration =>
        val superclass: Option[ClassType] = node.superclass
        val interfaces: Set[InterfaceType] = node.interfaces

        if (!superclass.isEmpty) {
            val env = mapping.mapping.get(node)
            if (!env.isEmpty) {
              val nameLookups : Seq[NameLookup]= superclass.get.name match {
                case s: SimpleName => Seq(NameLookup(s.value))
                case q: QualifiedName => q.value.map { i : InputString => NameLookup(i) }
              }
              nameLookups.foreach {
                case nameLookup : NameLookup =>
                  val ref = env.get.parent.get.lookup(nameLookup)
                  ref match {
                    case Some(ClassDeclaration(_, _, modifiers, _, _)) =>
                      if (modifiers.contains(FinalKeyword)) {
                        throw new SyntaxError("A class must not extend a final class.")
                      }
                    case Some(InterfaceDeclaration(_, _, _, _)) =>
                      throw new SyntaxError("A class must not extend an interface.")
                    case None => throw new SyntaxError("Declaration not found.")
                    case _ => Unit
                  }
                case _ => Unit
              }
            } else {
              throw new SyntaxError("ERROR: Should have been caught earlier.")
            }
        }

        val implement_names = interfaces.groupBy(x => x.name).mapValues(_.size)
        interfaces.foreach {
          case i : InterfaceType =>
            val env = mapping.mapping.get(node)
            if (!env.isEmpty) {
              val nameLookups : Seq[NameLookup]= i.name match {
                case s: SimpleName => Seq(NameLookup(s.value))
                case q: QualifiedName => q.value.map { i : InputString => NameLookup(i) }
              }
              nameLookups.foreach {
                case nameLookup : NameLookup =>
                  val ref = env.get.parent.get.lookup(nameLookup)
                  ref match {
                    case Some(ClassDeclaration(_, _, _, _, _)) =>
                      throw new SyntaxError("A class must not implement a class.")
                    case None => throw new SyntaxError("Declaration not found.")
                    case _ => Unit
                  }
                case _ => Unit
              }
            } else {
              throw new SyntaxError("ERROR: Should have been caught earlier.")
            }
            if (implement_names.get(i.name).get > 1) {
              throw new SyntaxError("An interface must not be repeated in an extends clause of an interface.")
            }

          case _ => Unit
        }

      case node: InterfaceDeclaration =>
        val interfaces: Set[InterfaceType] = node.interfaces

        val extend_names = interfaces.groupBy(x => x.name).mapValues(_.size)
        interfaces.foreach {
          case i : InterfaceType =>
            val env = mapping.mapping.get(node)
            if (!env.isEmpty) {
              val nameLookups : Seq[NameLookup]= i.name match {
                case s: SimpleName => Seq(NameLookup(s.value))
                case q: QualifiedName => q.value.map { i : InputString => NameLookup(i) }
              }
              nameLookups.foreach {
                case nameLookup : NameLookup =>
                  val ref = env.get.parent.get.lookup(nameLookup)
                  ref match {
                    case Some(ClassDeclaration(_, _, _, _, _)) =>
                      throw new SyntaxError("An interface must not extend a class.")
                    case None => throw new SyntaxError("Declaration not found.")
                    case _ => Unit
                  }
                case _ => Unit
              }
            } else {
              throw new SyntaxError("ERROR: Should have been caught earlier.")
            }
            if (extend_names.get(i.name).get > 1) {
              throw new SyntaxError("An interface must not be repeated in an extends clause of an interface.")
            }
          case _ => Unit
        }

      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }
}
