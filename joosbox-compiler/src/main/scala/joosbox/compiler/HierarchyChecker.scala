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
    InterfaceMemberDeclaration,
    ConstructorDeclaration,
    FieldDeclaration
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


  def checkClassHierarchy(klass: Option[ClassType], classList: List[Seq[InputString]], env: Environment) : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = {
    var methodList : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty
    if (!klass.isEmpty) {
      var klassList = klass.get.inputString :: classList
      val nameLookup : EnvironmentLookup = klass.get.name match {
        case s: SimpleName => NameLookup(s.value)
        case q: QualifiedName => QualifiedNameLookup(q)
      }
      val ref = env.lookup(nameLookup)
      ref match {
        case Some(ClassDeclaration(_, body, mods, superklass, interfaces)) =>
          methodList = body.declarations.map {
            case m: MethodDeclaration =>
              (MethodLookup(m.name, m.parameters.map(_.varType)), m)
            case c: ConstructorDeclaration =>
              (ConstructorLookup(c.parameters.map(_.varType)), c)
            case f: FieldDeclaration =>
              (IdentifierLookup(f.name), f)
          }
          interfaces.foreach{
            case i : InterfaceType =>
              methodList = methodList ++ HierarchyChecker.checkInterfaceHierarchy(i, List(Seq.empty), env)
          }
          if(!superklass.isEmpty) {
            if (klassList.contains(superklass.get.inputString)) {
              throw new SyntaxError("Class hierarchy must be acyclic.")
            }
            if (mods.contains(FinalKeyword)) {
              throw new SyntaxError("A class must not extend a final class.")
            }
            methodList = methodList ++ HierarchyChecker.checkClassHierarchy(superklass, klassList, env)
          }
        case Some(InterfaceDeclaration(_, _, _, _)) =>
          throw new SyntaxError("A class must not extend an interface.")
        case _ => throw new SyntaxError("A class can only extend a class.")
      }
    } else {
      // Implicit extends java.lang.Object
      val implicitSuperclass = QualifiedNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("Object"))))
      val implicitRef = env.lookup(implicitSuperclass)
      implicitRef match {
        case Some(ClassDeclaration(_, superbody, _, _, _)) =>
          methodList = methodList ++ superbody.declarations.map {
            case m: MethodDeclaration =>
              (MethodLookup(m.name, m.parameters.map(_.varType)), m)
            case c: ConstructorDeclaration =>
              (ConstructorLookup(c.parameters.map(_.varType)), c)
          }
        case _ => Unit
      }
    }
    methodList
  }

  def checkInterfaceHierarchy(interface: InterfaceType, interfaceList: List[Seq[InputString]], env: Environment) : Seq[(EnvironmentLookup, AbstractSyntaxNode)]  = {
    var methodList : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty
    var intList : List[Seq[InputString]] = interface.inputString :: interfaceList
    val nameLookup : EnvironmentLookup = interface.name match {
      case s: SimpleName => NameLookup(s.value)
      case q: QualifiedName => QualifiedNameLookup(q)
    }
    val ref = env.lookup(nameLookup)
    ref match {
      case Some(InterfaceDeclaration(_, body, _, interfaces)) =>
        methodList = body.declarations.map {
          case m: InterfaceMemberDeclaration =>
            (MethodLookup(m.name, m.parameters.map(_.varType)), m)
        }
        if(!interfaces.isEmpty) {
          interfaces.foreach {
            case i: InterfaceType =>
              if (intList.contains(i.inputString)) {
                throw new SyntaxError("Interface hierarchy must be acyclic.")
              }
              methodList = methodList ++ HierarchyChecker.checkInterfaceHierarchy(i, intList, env)
          }
        } else {
            // Implicit extends java.lang.Object as interface
            val implicitSuperinterface = QualifiedNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("Object"))))
            val implicitRef = env.lookup(implicitSuperinterface)
            implicitRef match {
              case Some(ClassDeclaration(_, superbody, _, _, _)) =>
                methodList = methodList ++ superbody.declarations.map {
                  case m: MethodDeclaration =>
                    (MethodLookup(m.name, m.parameters.map(_.varType)), m)
                  case c: ConstructorDeclaration => //Appeasement
                    (ConstructorLookup(c.parameters.map(_.varType)), c)
                }
              case _ => Unit
            }
          }
      case Some(ClassDeclaration(_, _, _, _, _)) =>
        throw new SyntaxError("An interface must not implement a class.")
      case _ => Unit
    }
    methodList
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    node match {
      case node: ClassDeclaration =>
        val name : InputString = node.name
        val modifiers: Set[Modifier] = node.modifiers
        val superclass: Option[ClassType] = node.superclass
        val interfaces: Seq[InterfaceType] = node.interfaces
        var declarations : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty
        var superDeclarations : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty
        var intDeclarations : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty

        // Check class declarations
        declarations = node.body.declarations.map {
            case m: MethodDeclaration =>
              if (m.modifiers.contains(AbstractKeyword)) {
                if (!modifiers.contains(AbstractKeyword)) {
                  throw new SyntaxError("Abstract methods must be defined in abstract classes/interfaces.")
                }
              }
              (MethodLookup(m.name, m.parameters.map(_.varType)), m)
            case c: ConstructorDeclaration =>
              (ConstructorLookup(c.parameters.map(_.varType)), c)
            case f: FieldDeclaration =>
              (IdentifierLookup(f.name), f)
        }


        val env = mapping.enclosingScopeOf(node).get
        val classEnv = mapping.enclosingScopeOf(node.body).get

        // Check extends hierarchy
        val implicitSuperclass = QualifiedNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"), InputString("Object"))))
        val implicitRef = env.lookup(implicitSuperclass).get
        val ref = env.lookup(NameLookup(name)).get 
        // If we are checking java.lang.Object, dont
        if (implicitRef != ref) {
          superDeclarations = HierarchyChecker.checkClassHierarchy(superclass, List(Seq(name)), env)
        }

        // Check the implemented interfaces
        var interfaceNodes : List[String] = List.empty[String]
        interfaces.foreach {
          case i : InterfaceType =>
            intDeclarations = intDeclarations ++ HierarchyChecker.checkInterfaceHierarchy(i, List(Seq()), env)

            // Below is just for checking 'duplicate' ints in implements list
            val nameLookup : EnvironmentLookup = i.name match {
              case s: SimpleName => NameLookup(s.value)
              case q: QualifiedName => QualifiedNameLookup(q)
            }
            val ref = env.lookup(nameLookup)
            ref match {
              case Some(InterfaceDeclaration(_, intBody, mods, ints)) =>
                if (interfaceNodes.contains(ref.get.asInstanceOf[InterfaceDeclaration].name.filename)) {
                  throw new SyntaxError("An interface must not be repeated in a implements clause of a class.")
                }
                interfaceNodes = ref.get.asInstanceOf[InterfaceDeclaration].name.filename :: interfaceNodes
              case _ => Unit
            }
        }

        // For each super method, look for it in this class' scope
        superDeclarations.foreach {
          case (superMethodLookup : MethodLookup, superMethod : MethodDeclaration) =>
            var implemented : Boolean = false
            declarations.foreach {
              case (methodLookup : MethodLookup, method : MethodDeclaration) =>
                if (superMethodLookup == methodLookup) {
                  implemented = true
                  if (superMethod.memberType != method.memberType) {
                    throw new SyntaxError("A method must not replace a method with a different return type.")
                  }
                  if (superMethod.modifiers.contains(FinalKeyword)) {
                    throw new SyntaxError("A method must not replace a final method.")
                  }
                  if (superMethod.modifiers.contains(StaticKeyword)) {
                    if (!method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A nonstatic method must not replace a static method.")
                    }
                  }
                  if (!superMethod.modifiers.contains(StaticKeyword)) {
                    if (method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A static method must not replace an instance method.")
                    }
                  }
                  if (superMethod.modifiers.contains(PublicKeyword)) {
                    if (method.modifiers.contains(ProtectedKeyword)) {
                      throw new SyntaxError("A protected method must not replace a public method.")
                    }
                  }
                }

              case _ => Unit
            }
            if (superMethod.modifiers.contains(AbstractKeyword)) {
              if (!modifiers.contains(AbstractKeyword) && !implemented) {
                throw new SyntaxError("Extending classes with abstract methods must either be an abstract class or implement the method.")
              }
            }

          // TODO copypastad from above
          case (superMethodLookup : MethodLookup, superMethod : InterfaceMemberDeclaration) =>
            var implemented : Boolean = false
            declarations.foreach {
              case (methodLookup : MethodLookup, method : MethodDeclaration) =>
                if (superMethodLookup == methodLookup) {
                  implemented = true
                  if (superMethod.memberType != method.memberType) {
                    throw new SyntaxError("A method must not replace a method with a different return type.")
                  }
                  if (superMethod.modifiers.contains(FinalKeyword)) {
                    throw new SyntaxError("A method must not replace a final method.")
                  }
                  if (superMethod.modifiers.contains(StaticKeyword)) {
                    if (!method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A nonstatic method must not replace a static method.")
                    }
                  }
                  if (!superMethod.modifiers.contains(StaticKeyword)) {
                    if (method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A static method must not replace an instance method.")
                    }
                  }
                  if (superMethod.modifiers.contains(PublicKeyword)) {
                    if (method.modifiers.contains(ProtectedKeyword)) {
                      throw new SyntaxError("A protected method must not replace a public method.")
                    }
                  }
                }
              case _ => Unit
            }
            if (!modifiers.contains(AbstractKeyword) && !implemented) {
              throw new SyntaxError("Extending classes with abstract methods must either be an abstract class or implement the method.")
            }



          case _ => Unit
        }


        // For each interface method, look for it in this class' scope
        intDeclarations.foreach {
          case (memberLookup : MethodLookup, intMember : InterfaceMemberDeclaration) =>
            var implemented : Boolean = false
            (declarations ++ superDeclarations).foreach {
              case (methodLookup : MethodLookup, method : MethodDeclaration) =>
                if (memberLookup == methodLookup) {
                  implemented = true
                  if (intMember.memberType != method.memberType) {
                    throw new SyntaxError("A method must not replace a method with a different return type.")
                  }
                  if (intMember.modifiers.contains(FinalKeyword)) {
                    throw new SyntaxError("A method must not replace a final method.")
                  }
                  if (intMember.modifiers.contains(StaticKeyword)) {
                    if (!method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A nonstatic method must not replace a static method.")
                    }
                  }
                  if (!intMember.modifiers.contains(StaticKeyword)) {
                    if (method.modifiers.contains(StaticKeyword)) {
                      throw new SyntaxError("A static method must not replace an instance method.")
                    }
                  }
 
                  if (intMember.modifiers.contains(PublicKeyword)) {
                    if (method.modifiers.contains(ProtectedKeyword)) {
                      throw new SyntaxError("A protected method must not replace a public method.")
                    }
                  }
                }
              case _ => Unit
            }
            if (!modifiers.contains(AbstractKeyword) && !implemented) {
              throw new SyntaxError("Implementing interfaces with abstract methods must either be an abstract class or implement the method.")
            }

          case _ => Unit
        }

        // For each interface method, look for it in this class' scope
        intDeclarations.foreach {
          case (memberLookup : MethodLookup, intMember : InterfaceMemberDeclaration) =>
            var implemented : Boolean = false
            (intDeclarations).foreach {
              case (memberLookup2 : MethodLookup, intMember2 : InterfaceMemberDeclaration) =>
                if (memberLookup == memberLookup2) {
                  if (intMember.memberType != intMember2.memberType) {
                    throw new SyntaxError("Class implements two incompatible interfaces")
                  }
                }

              case _ => Unit
            }

          case _ => Unit
        }



      case node: InterfaceDeclaration =>
        val name : InputString = node.name
        val interfaces: Seq[InterfaceType] = node.interfaces
        val modifiers: Set[Modifier] = node.modifiers

        val env = mapping.enclosingScopeOf(node).get
        val interfaceEnv = mapping.enclosingScopeOf(node.body).get

        var declarations : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty
        var intDeclarations : Seq[(EnvironmentLookup, AbstractSyntaxNode)] = Seq.empty

        declarations = node.body.declarations.map {
          case m: InterfaceMemberDeclaration =>
            (MethodLookup(m.name, m.parameters.map(_.varType)), m)
        }

        intDeclarations = HierarchyChecker.checkInterfaceHierarchy(InterfaceType(SimpleName(name)), List(Seq()), env)

        // Below is just for checking 'duplicate' ints in extends list
        var interfaceNodes : List[String] = List.empty[String]
        interfaces.foreach {
          case i : InterfaceType =>
            val nameLookup : EnvironmentLookup = i.name match {
              case s: SimpleName => NameLookup(s.value)
              case q: QualifiedName => QualifiedNameLookup(q)
            }
            val ref = env.lookup(nameLookup)
            ref match {
              case Some(InterfaceDeclaration(_, intBody, mods, ints)) =>
                if (interfaceNodes.contains(ref.get.asInstanceOf[InterfaceDeclaration].name.filename)) {
                  throw new SyntaxError("An interface must not be repeated in a extends clause of a class.")
                }
                interfaceNodes = ref.get.asInstanceOf[InterfaceDeclaration].name.filename :: interfaceNodes
              case _ => Unit
            }
        }

        // For each super method, look for it in this interface's scope
        intDeclarations.foreach {
          case (methodLookup : MethodLookup, superMethod : InterfaceMemberDeclaration) =>
            val ref = interfaceEnv.lookup(methodLookup)
            ref match {
              case Some(m : InterfaceMemberDeclaration) =>
                if (superMethod.memberType != m.memberType) {
                  throw new SyntaxError("A method must not replace a method with a different return type.")
                }
                if (superMethod.modifiers.contains(FinalKeyword)) {
                  throw new SyntaxError("A method must not replace a final method.")
                }
                if (superMethod.modifiers.contains(StaticKeyword)) {
                  if (!m.modifiers.contains(StaticKeyword)) {
                    throw new SyntaxError("A nonstatic method must not replace a static method.")
                  }
                }
                if (!superMethod.modifiers.contains(StaticKeyword)) {
                  if (m.modifiers.contains(StaticKeyword)) {
                    throw new SyntaxError("A static method must not replace an instance method.")
                  }
                }
                if (superMethod.modifiers.contains(PublicKeyword)) {
                  if (m.modifiers.contains(ProtectedKeyword)) {
                    throw new SyntaxError("A protected method must not replace a public method.")
                  }
                }
              case None =>
                if (superMethod.modifiers.contains(AbstractKeyword)) {
                  if (!modifiers.contains(AbstractKeyword)) {
                    throw new SyntaxError("Extending interfaces with abstract methods must either be an abstract interface or implement the method.")
                  }
                }
              case _ => Unit
            }
          // TODO: Clean this up (copy-pasta'd from above -- handles implicit case
          case (methodLookup : MethodLookup, superMethod : MethodDeclaration) =>
            val ref = interfaceEnv.lookup(methodLookup)
            ref match {
              case Some(m : InterfaceMemberDeclaration) =>
                if (superMethod.memberType != m.memberType) {
                  throw new SyntaxError("A method must not replace a method with a different return type.")
                }
                if (superMethod.modifiers.contains(FinalKeyword)) {
                  throw new SyntaxError("A method must not replace a final method.")
                }
                if (superMethod.modifiers.contains(StaticKeyword)) {
                  if (!m.modifiers.contains(StaticKeyword)) {
                    throw new SyntaxError("A nonstatic method must not replace a static method.")
                  }
                }
                if (!superMethod.modifiers.contains(StaticKeyword)) {
                  if (m.modifiers.contains(StaticKeyword)) {
                    throw new SyntaxError("A static method must not replace an instance method.")
                  }
                }
                if (superMethod.modifiers.contains(PublicKeyword)) {
                  if (m.modifiers.contains(ProtectedKeyword)) {
                    throw new SyntaxError("A protected method must not replace a public method.")
                  }
                }
              case None =>
                if (superMethod.modifiers.contains(AbstractKeyword)) {
                  if (!modifiers.contains(AbstractKeyword)) {
                    throw new SyntaxError("Extending interfaces with abstract methods must either be an abstract interface or implement the method.")
                  }
                }
              case _ => Unit
            }
          case _ => Unit
        }



      case _ => Unit
    }

    node.children.foreach { node => check(node) }
  }
}
