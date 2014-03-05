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

  -* A class or interface must not declare two methods with the same signature.

  -* A class must not declare two constructors with the same parameter type.

  -* A class or interface must not contain (declare or inherit) two methods with
    the same signature but different return types

  -* A class that contains (declares or inherits) any abstract methods must be
    abstract.

  -* A nonstatic method must not replace a static method.

  -* A method must not replace a method with a different return type.

  -* A protected method must not replace a public method.

  -* A method must not replace a final method.
*/

object HierarchyChecker {
  import AbstractSyntaxNode.{
    InterfaceDeclaration,
    ClassDeclaration,
    InterfaceType,
    ClassType,
    Modifier,
    SimpleName,
    QualifiedName,
    FinalKeyword,
    AbstractKeyword,
    StaticKeyword,
    PublicKeyword,
    ProtectedKeyword,
    ClassBody,
    ClassBodyDeclaration,
    MethodDeclaration,
    InterfaceMemberDeclaration
  }

  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      HierarchyChecker.check(unit)(mapping)
    }
    Map.empty
  }


  def checkClassHierarchy(klass: Option[ClassType], classList: List[Seq[InputString]], env: Environment) {
    if (!klass.isEmpty) {
      var klassList = klass.get.inputString :: classList
      val nameLookup : EnvironmentLookup = klass.get.name match {
        case s: SimpleName => NameLookup(s.value)
        case q: QualifiedName => QualifiedNameLookup(q)
      }
      val ref = env.lookup(nameLookup)
      ref match {
        case Some(ClassDeclaration(_, _, _, superklass, _)) =>
          if(!superklass.isEmpty) {
            if (klassList.contains(superklass.get.inputString)) {
              throw new SyntaxError("Class hierarchy must be acyclic.")
            }
            checkClassHierarchy(superklass, klassList, env)
          } // If superklass is Empty, java.lang.Object is implicit superclass
        case _ => Unit
      }
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
      case node: ClassDeclaration =>
        val name : InputString = node.name
        val modifiers: Set[Modifier] = node.modifiers
        val superclass: Option[ClassType] = node.superclass
        val interfaces: Set[InterfaceType] = node.interfaces
        val declarations : Seq[ClassBodyDeclaration] = node.body.declarations
        var superDeclarations : Seq[ClassBodyDeclaration] = Seq.empty
        var intDeclarations : Seq[InterfaceMemberDeclaration] = Seq.empty

        val env = mapping.enclosingScopeOf(node).get

        // Check extends hierarchy
        HierarchyChecker.checkClassHierarchy(superclass, List(Seq(name)), env)

        // Check class declarations
        declarations.foreach {
          case m : MethodDeclaration =>
            if (m.modifiers.contains(AbstractKeyword)) {
              if (!modifiers.contains(AbstractKeyword)) {
                throw new SyntaxError("Abstract methods must be defined in abstract classes/interfaces.")
              }
            }
          case _ => Unit
        }

        // Direct superclass checks
        if (!superclass.isEmpty) {
          val nameLookup : EnvironmentLookup = superclass.get.name match {
            case s: SimpleName => NameLookup(s.value)
            case q: QualifiedName => QualifiedNameLookup(q)
          }
          val ref = env.lookup(nameLookup)
          ref match {
            case Some(ClassDeclaration(_, superBody, superModifiers, _, _)) =>
              if (superModifiers.contains(FinalKeyword)) {
                throw new SyntaxError("A class must not extend a final class.")
              }

              superDeclarations = superBody.declarations
            case Some(InterfaceDeclaration(_, _, _, _)) =>
              throw new SyntaxError("A class must not extend an interface.")
            case None => throw new SyntaxError("Extended declaration not found.")
            case _ => throw new SyntaxError("A class can only extend a class.")

          }

          val superAST = ref.get.asInstanceOf[AbstractSyntaxNode]
          val superEnv = mapping.enclosingScopeOf(superAST).get
          superDeclarations.foreach {
            case superMethod : MethodDeclaration =>
              val methodLookup = MethodLookup(superMethod.name, superMethod.parameters.map(_.varType))
              val ref = superEnv.lookup(methodLookup)
              ref match {
                case Some(MethodDeclaration(_, mods, retType, _, _)) =>
                  if (superMethod.memberType != retType) {
                    throw new SyntaxError("A method must not replace a method with a different return type.")
                  }
                  if (superMethod.modifiers.contains(FinalKeyword)) {
                    throw new SyntaxError("A method must not replace a final method.")
                  }
                  if (superMethod.modifiers.contains(StaticKeyword)) {
                    if (!mods.contains(StaticKeyword)) {
                      throw new SyntaxError("A nonstatic method must not replace a static method.")
                    }
                  }
                  if (superMethod.modifiers.contains(PublicKeyword)) {
                    if (mods.contains(ProtectedKeyword)) {
                      throw new SyntaxError("A protected method must not replace a public method.")
                    }
                  }
                  if (superMethod.modifiers.contains(AbstractKeyword)) {
                    if (!modifiers.contains(AbstractKeyword)) {
                      throw new SyntaxError("Extending classes with abstract methods must either be an abstract class or implement the method.")
                    }
                  }
                case _ => Unit
              }
            case _ => Unit
          }


        }

        // Check the implented interfaces
        val implement_names = interfaces.groupBy(x => x.name).mapValues(_.size)
        interfaces.foreach {
          case i : InterfaceType =>
            val nameLookup : EnvironmentLookup = i.name match {
              case s: SimpleName => NameLookup(s.value)
              case q: QualifiedName => QualifiedNameLookup(q)
            }
            val ref = env.lookup(nameLookup)
            ref match {
              case Some(InterfaceDeclaration(_, intBody, mods, ints)) =>
                intDeclarations = intBody.declarations
              case Some(ClassDeclaration(_, _, _, _, _)) =>
                throw new SyntaxError("A class must not implement a class.")
              case None => throw new SyntaxError("Implemented declaration not found.")
              case _ => Unit
            }

            if (implement_names.get(i.name).get > 1) {
              throw new SyntaxError("An interface must not be repeated in an extends clause of an interface.")
            }

            val intAST = ref.get.asInstanceOf[AbstractSyntaxNode]
            val intEnv = mapping.enclosingScopeOf(intAST).get
            intDeclarations.foreach {
              case intMember : InterfaceMemberDeclaration =>
                val memberLookup = MethodLookup(intMember.name, intMember.parameters.map(_.varType))
                val ref = intEnv.lookup(memberLookup)
                ref match {
                  case Some(MethodDeclaration(_, mods, retType, _, _)) =>
                    if (intMember.memberType != retType) {
                      throw new SyntaxError("A method must not replace a method with a different return type.")
                    }
                    if (intMember.modifiers.contains(FinalKeyword)) {
                      throw new SyntaxError("A method must not replace a final method.")
                    }
                    if (intMember.modifiers.contains(StaticKeyword)) {
                      if (!mods.contains(StaticKeyword)) {
                        throw new SyntaxError("A nonstatic method must not replace a static method.")
                      }
                    }
                    if (intMember.modifiers.contains(PublicKeyword)) {
                      if (mods.contains(ProtectedKeyword)) {
                        throw new SyntaxError("A protected method must not replace a public method.")
                      }
                    }
                    if (intMember.modifiers.contains(AbstractKeyword)) {
                      if (!modifiers.contains(AbstractKeyword)) {
                        throw new SyntaxError("Extending classes with abstract methods must either be an abstract class or implement the method.")
                      }
                    }

                  case _ => Unit
                }
              case _ => Unit
            }

          case _ => Unit
        }

      case node: InterfaceDeclaration =>
        val interfaces: Set[InterfaceType] = node.interfaces

        val extend_names = interfaces.groupBy(x => x.name).mapValues(_.size)
        interfaces.foreach {
          case i : InterfaceType =>
            val env = mapping.enclosingScopeOf(node)
            if (!env.isEmpty) {
              val nameLookup : EnvironmentLookup = i.name match {
                case s: SimpleName => NameLookup(s.value)
                case q: QualifiedName => QualifiedNameLookup(q)
              }
              val ref = env.get.lookup(nameLookup)
              ref match {
                case Some(ClassDeclaration(_, _, _, _, _)) =>
                  throw new SyntaxError("An interface must not extend a class.")
                case None => throw new SyntaxError("Extended declaration not found.")
                case _ => Unit
              }
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
