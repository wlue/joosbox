package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import joosbox.parser._

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object TypeChecker {
  import AbstractSyntaxNode._

  def link(units: Seq[CompilationUnit]): Map[Any, Referenceable] = {
    units.foreach { unit => TypeChecker.check(unit) }
    Map.empty
  }

  def resolveTypeName(name: TypeName, env: Environment): Option[Type] = {
    env.lookup(TypeNameLookup(name.toQualifiedName)) match {
      case None => throw new SyntaxError("TypeName " + name.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case _: ClassDeclaration => Some(ClassType(name))
        case _: InterfaceDeclaration => Some(InterfaceType(name))
        case _ => throw new SyntaxError("Name " + name.niceName + " does not resolve to a class or interface type.")
      }
    }
  }

  def resolveExpressionName(name: ExpressionName, env: Environment): Option[Type] = {
    var tmpEnv = env
    if (!name.prefix.isEmpty) {
      val t:Option[Type] = name.prefix.get match {
        case n:ExpressionName => resolveExpressionName(n, env)
        case other => resolveType(other)
      }

      t match {
        case None => throw new SyntaxError("ExpressionName " + name.prefix.get.niceName + " does not resolve to a type.")
        case Some(result) => result match {
          case x: ClassOrInterfaceType => {
            val xNode = env.lookup(TypeNameLookup(x.name.toQualifiedName)).get match {
              case cd: ClassDeclaration => cd.body
              case id: InterfaceDeclaration => id.body
              case _ => throw new SyntaxError("Invalid declaration.")
            }
            val xEnv = xNode.scope.get
            tmpEnv = xEnv
          }

          case c: ClassType => {
            val cNode = env.lookup(TypeNameLookup(c.name.toQualifiedName)).get match {
              case cd: ClassDeclaration => cd.body
              case id: InterfaceDeclaration => id.body
              case _ => throw new SyntaxError("Invalid declaration.")
            }
            val cEnv = cNode.scope.get
            tmpEnv = cEnv
          }

          case i: InterfaceType => {
            val iNode = env.lookup(TypeNameLookup(i.name.toQualifiedName)).get match {
              case cd: ClassDeclaration => cd.body
              case id: InterfaceDeclaration => id.body
              case _ => throw new SyntaxError("Invalid declaration.")
            }
            val iEnv = iNode.scope.get
            tmpEnv = iEnv
          }

          case a: ArrayType => {
            if (name.value == InputString("length")) {
              return Some(IntKeyword)
            } else {
              throw new SyntaxError("Only valid array field is length.")
            }
          }

          case _ => throw new SyntaxError("Error: Resolved prefix to non-reference type")
        }
      }
    }

    val lookup = ExpressionNameLookup(ExpressionName(name.value).toQualifiedName)
    tmpEnv.lookup(lookup) match {
      case None =>
        throw new SyntaxError("ExpressionName " + name.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case p: FormalParameter => Some(p.varType)
        case f: FieldDeclaration => Some(f.memberType)
        case v: LocalVariableDeclaration => Some(v.memberType)
        case v: ForVariableDeclaration => Some(v.typeDeclaration)
        case _ => throw new SyntaxError("Could not resolve type of expression.");
      }
    }
  }

  def resolvedTypesForArgs(args: Seq[Expression]): Seq[Type] = {
    args.map { arg =>
      resolveType(arg) match {
        case Some(argType) => argType
        case None => throw new SyntaxError("Could not resolve type for argument \"" + arg + "\"")
      }
    }
  }

  def resolvePrimaryAndFindScope(ref:Primary, env:Environment) : Environment = {
    val primaryType: Option[Type] = resolveType(ref)
    if (primaryType.isEmpty) {
      throw new SyntaxError("Attempted access of unresolvable type.")
    }

    val typeName: TypeName = primaryType.get match {
      case c: ClassType => c.name
      case i: InterfaceType => i.name
      case _ => throw new SyntaxError("Can't perform access on non-class, non-interface type.")
    }

    env.lookup(TypeNameLookup(typeName.toQualifiedName)) match {
      case None => throw new SyntaxError("Name " + typeName.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case c: ClassDeclaration => c.body.scope.get
        case i: InterfaceDeclaration => i.body.scope.get
        case _ => throw new SyntaxError("Name " + typeName.niceName + " does not resolve to a class or interface type.")
      }
    }
  }

  def compatibleTypes(type1: Type, type2: Type): Boolean = (type1, type2) match {
    // Exact types are compatible.
    case (one, two) if one == two => true
    // Reference types are compatible.
    case (one: ReferenceType, two: ReferenceType) => true
    // Numeric types are always compatible with each other.
    case (one: NumericType, two: NumericType) => true
    case _ => false
  }

  /**
   * Compare two types. If they are compatible, return the type or the resolve type.
   * Otherwise, throw a type mismatch exception.
   */
  def matchCompatibleType(type1: Option[Type], type2: Option[Type], resolve: Option[Type] = None): Option[Type] = {
    (type1, type2) match {
      case (Some(type1), Some(type2)) if compatibleTypes(type1, type2) => {
        if (resolve.isEmpty) {
          Some(type1)
        } else {
          resolve
        }
      }
      case _ => throw new SyntaxError("Type mismatch. (" + type1 + ") and (" + type2 + ")");
    }
  }

  /**
   * Compare two types. If they are identical, return the type or the resolve type.
   * Otherwise, throw a type mismatch exception.
   */
  def matchIdenticalType(type1: Option[Type], type2: Option[Type], resolve: Option[Type] = None): Option[Type] = {
    (type1, type2) match {
      case (Some(type1), Some(type2)) if type1 == type2 => {
        if (resolve.isEmpty) {
          Some(type1)
        } else {
          resolve
        }
      }
      case _ => throw new SyntaxError("Type mismatch. (" + type1 + ") and (" + type2 + ")");
    }
  }

  def promotedTypeOption(tpe: Option[Type]): Option[Type] = tpe.map(promotedType(_))

  /**
   * Widening conversions used to promote types.
   * http://www.cs.cornell.edu/andru/javaspec/5.doc.html#170952
   */
  def promotedType(tpe: Type): Type = tpe match {
    // Numeric promotion. Byte, Char, and Short get promoted to Int.
    case ShortKeyword => IntKeyword
    case ByteKeyword => IntKeyword
    case CharKeyword => IntKeyword
    case _ => tpe
  }

  def resolveType(node: AbstractSyntaxNode): Option[Type] = {
    node match {
      case t: Type => Some(t)

      case TrueLiteral => Some(BooleanKeyword)
      case FalseLiteral => Some(BooleanKeyword)
      case ThisKeyword =>
        val env = node.scope.get
        env.getEnclosingClassNode match {
          case Some(declaration: ClassDeclaration) => resolveType(declaration.name)
          case _ => throw new SyntaxError("this resolved to non-class declaration.")
        }

      case Num(value, _) => Some(IntKeyword)
      case _: CharLiteral => Some(CharKeyword)
      case _: StringLiteral => Some(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName))

      case param: FormalParameter => Some(param.varType)
      case field: FieldDeclaration => Some(field.memberType)
      case variable: LocalVariableDeclaration => Some(variable.memberType)
      case variable: ForVariableDeclaration => Some(variable.typeDeclaration)

      case expr: Expression => expr match {
        case c: CastExpression => Some(c.targetType)
        case ParenthesizedExpression(expression) => resolveType(expression)

        case e: PostfixExpression => e match {
          case name: ExpressionName => {
            var env = node.scope.get
            if (!name.prefix.isEmpty) {
              NameLinker.disambiguateName(name)(env) match {
                case n: ExpressionName => resolveExpressionName(n, env)
                case _ => throw new SyntaxError("Expression name " + name.niceName + " disambiguates weird.")
              }
            } else {
              resolveExpressionName(name, env)
            }
          }

          case name: TypeName => {
            val env = node.scope.get
            resolveTypeName(name, env)
          }

          // TODO - I think these should be None, but not sure
          case name: PackageName => throw new SyntaxError("PackageName" + name.niceName)
          case name: MethodName => throw new SyntaxError("MethodName" + name.niceName)
          case name: AmbiguousName => throw new SyntaxError("AmbiguousName" + name.niceName)

          case _ => throw new SyntaxError("PostfixExpression")
        }

        case conditional: ConditionalExpression => conditional match {
          // Eager boolean is supported, bitwise are not
          case OrExpression(_, _) => Some(BooleanKeyword)
          case AndExpression(_, _) => Some(BooleanKeyword)
          // Binary expression are supported
          case BinOrExpression(_, _) => Some(BooleanKeyword)
          case BinXorExpression(_, _) => Some(BooleanKeyword)
          case BinAndExpression(_, _) => Some(BooleanKeyword)
        }

        case relational: RelationalExpression => relational match {
          case EqualExpression(e1, e2)        => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case NotEqualExpression(e1, e2)     => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case LessThanExpression(e1, e2)     => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case LessEqualExpression(e1, e2)    => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case GreaterThanExpression(e1, e2)  => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case GreaterEqualExpression(e1, e2) => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword))
          case InstanceOfExpression(expr, reference) => {
            (resolveType(expr), reference) match {
              case (Some(_: ReferenceType), _: ReferenceType) => Some(BooleanKeyword)
              case (left, _) => throw new SyntaxError("Left hand side of \"instanceOf\" is not a reference: " + left)
            }
          }
        }

        case arithmetic: ArithmeticExpression => arithmetic match {
          case AddExpression(e1, e2)        => promotedTypeOption(matchCompatibleType(resolveType(e1), resolveType(e2)))
          case SubtractExpression(e1, e2)   => promotedTypeOption(matchCompatibleType(resolveType(e1), resolveType(e2)))
          case MultiplyExpression(e1, e2)   => promotedTypeOption(matchCompatibleType(resolveType(e1), resolveType(e2)))
          case DivideExpression(e1, e2)     => promotedTypeOption(matchCompatibleType(resolveType(e1), resolveType(e2)))
          case ModExpression(e1, e2)        => promotedTypeOption(matchCompatibleType(resolveType(e1), resolveType(e2)))
        }

        case NegatedExpression(e1) => promotedTypeOption(resolveType(e1))

        case FieldAccess(ref, name) => {
          val env = node.scope.get
          val scope:Environment = resolvePrimaryAndFindScope(ref, env)
          val fieldName: ExpressionName = ExpressionName(name)

          scope.lookup(ExpressionNameLookup(fieldName.toQualifiedName)) match {
            case None => throw new SyntaxError("Accessing " + fieldName.niceName + " does not resolve.")
            case Some(result) => result match {
              case p: FormalParameter => Some(p.varType)
              case f: FieldDeclaration => Some(f.memberType)
              case v: LocalVariableDeclaration => Some(v.memberType)
              case v: ForVariableDeclaration => Some(v.typeDeclaration)
              case _ => throw new SyntaxError("Accessing non-field!!!")
            }
          }
        }

        case SimpleArrayAccess(name, _) => resolveType(name)
        case ComplexArrayAccess(ref, expr) => throw new SyntaxError("ComplexArrayAccess")

        case ArrayCreationPrimary(t, _) => Some(t)
        case ClassCreationPrimary(t, _) => Some(t)

        case method: MethodInvocation => method match {
          case SimpleMethodInvocation(name, args) => {
            var env = node.scope.get

            val n = NameLinker.disambiguateName(name)(env).asInstanceOf[MethodName]
            n.prefix.foreach { namePrefix =>
              val typeOption: Option[Type] = namePrefix match {
                case name: ExpressionName => resolveExpressionName(name, env)
                case name: TypeName => resolveTypeName(name, env)
                case _ => throw new SyntaxError("Invalid prefix for MethodName: " + namePrefix)
              }

              env = typeOption match {
                case Some(ClassType(n)) =>
                  env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
                case Some(InterfaceType(n)) =>
                  env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
                case Some(ClassOrInterfaceType(n)) =>
                  env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
                case _ => throw new SyntaxError("Couldn't resolve type " + typeOption + ".")
              }
            }

            val argTypes: Seq[Type] = resolvedTypesForArgs(args)
            env.lookup(MethodLookup(QualifiedName(Seq(name.value)), argTypes)) match {
              case None => throw new SyntaxError("Invoking " + name.niceName + " does not resolve.")
              case Some(result) => result match {
                case method: MethodDeclaration => Some(method.memberType)
                case method: InterfaceMemberDeclaration => Some(method.memberType)
                case _ => throw new SyntaxError("Invoking non-method!!!")
              }
            }
          }

          case ComplexMethodInvocation(ref, name, args) => {
            val argTypes: Seq[Type] = resolvedTypesForArgs(args)
            val env = node.scope.get
            val scope:Environment = resolvePrimaryAndFindScope(ref, env)

            scope.lookup(MethodLookup(name.toQualifiedName, argTypes)) match {
              case None => throw new SyntaxError("Invoking complex" + name.niceName + " does not resolve.")
              case Some(result) => result match {
                case method: MethodDeclaration => Some(method.memberType)
                case method: InterfaceMemberDeclaration => Some(method.memberType)
                case _ => throw new SyntaxError("Invoking non-method!!!")
              }
            }
          }
        }

        case _ => None
      }

      // Some nodes just don't make sense to every look/check for type
      case _ =>  None
    }
  }

  def checkLogicalExpression(e1: Expression, e2: Expression) = {
    resolveType(e1) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
    resolveType(e2) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
  }

  def check(node: AbstractSyntaxNode) {
    val env = node.scope.get
    node match {
      // Check that no bitwise operations occur.
      case AndExpression(e1, e2) => TypeChecker.checkLogicalExpression(e1, e2)
      case OrExpression(e1, e2) => TypeChecker.checkLogicalExpression(e1, e2)

      // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
      case ThisKeyword => resolveType(node)

      // Check that fields/methods accessed as static are actually static, and that fields/methods
      // accessed as non-static are actually non-static.
      case method: SimpleMethodInvocation => {
        resolveType(method)
      }

      case c: ClassCreationPrimary => {
        val className: TypeName = c.classType.name
        env.lookup(TypeNameLookup(className.toQualifiedName)) match {
          // Check that no objects of abstract classes are created.
          case Some(klass: ClassDeclaration) => {
            if (klass.modifiers.contains(AbstractKeyword)) {
              throw new SyntaxError("Can't create instances of an abstract class.")
            }
          }
          case _ => {}
        }
        /*
        val types: Seq[Type] = resolvedTypesForArgs(c.args)
        val lookup: EnvironmentLookup = ConstructorLookup(className.value, types)
        if (env.lookup(lookup).isEmpty) {
          throw new SyntaxError("Can't find constructor matching arguments: " + className.niceName + "(" + types.mkString(",") + ")")
        }
        */
      }
      case _ => {}
    }
    node.children.foreach { node => check(node) }
  }
}
