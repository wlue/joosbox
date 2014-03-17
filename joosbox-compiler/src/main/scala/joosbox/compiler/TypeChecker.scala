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

  def resolveMethodName(
    ambiguousName: MethodName,
    args: Seq[Expression],
    env: Environment
  ): Option[TypeMethodDeclaration] = {
    val name: MethodName = NameLinker.disambiguateName(ambiguousName)(env).asInstanceOf[MethodName]

    val updatedEnv: Environment = name.prefix.map { namePrefix =>
      val typeOption: Option[Type] = namePrefix match {
        case name: ExpressionName => resolveExpressionName(name, env)
        case name: TypeName => resolveTypeName(name, env)
        case _ => throw new SyntaxError("Invalid prefix for MethodName: " + namePrefix)
      }

      typeOption match {
        case Some(ClassType(n)) =>
          env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
        case Some(InterfaceType(n)) =>
          env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
        case Some(ClassOrInterfaceType(n)) =>
          env.lookup(TypeNameLookup(n.toQualifiedName)).get.scope.get
        case _ => throw new SyntaxError("Couldn't resolve type " + typeOption + ".")
      }
    } getOrElse(env)

    val argTypes: Seq[Type] = resolvedTypesForArgs(args, updatedEnv)
    val lookup = MethodLookup(QualifiedName(Seq(name.value)), argTypes)

    updatedEnv.lookup(lookup) match {
      case Some(result) => result match {
        case method: MethodDeclaration => Some(method)
        case method: InterfaceMemberDeclaration => Some(method)
        case _ => throw new SyntaxError("Found non-method " + result)
      }
      case _ => None
    }
  }

  def withScope[T <: Type](t: T, s: Option[Environment]): T = {
    if (t.scope == None) {
      t.scope = s
    }
    t
  }

  def resolveTypeName(name: TypeName, env: Environment): Option[Type] = {
    env.lookup(TypeNameLookup(name.toQualifiedName)) match {
      case None => throw new SyntaxError("TypeName " + name.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case _: ClassDeclaration => Some(withScope(ClassType(name), name.scope))
        case _: InterfaceDeclaration => Some(withScope(InterfaceType(name), name.scope))
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
        case None =>
          throw new SyntaxError("ExpressionName " + name.prefix.get + " does not resolve to a type.")
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
              return Some(IntKeyword())
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
        throw new SyntaxError("ExpressionName " + name + " does not resolve to a type.")
      case Some(result) => result match {
        case p: FormalParameter => Some(p.varType)
        case f: FieldDeclaration => Some(f.memberType)
        case v: LocalVariableDeclaration => Some(v.memberType)
        case v: ForVariableDeclaration => Some(v.typeDeclaration)
        case c: ClassDeclaration => Some(withScope(ClassType(c.name), name.scope))
        case x =>
          throw new SyntaxError("Could not resolve type of expression: " + x);
      }
    }
  }

  def resolvedTypesForArgs(args: Seq[Expression], env:Environment): Seq[Type] = {
    args.map {
      arg => {
        val result = resolveType(arg) match {
          case Some(argType) => argType match {
            case c:ClassType =>
              env.lookup(TypeNameLookup(c.name.toQualifiedName)).get match {
                case decl: ClassDeclaration =>
                  withScope(ClassType(decl.fullyQualifiedName.get), Some(env))
                case _ => throw new SyntaxError("Bad fully qualified type.")
              }
            case i:InterfaceType =>
              env.lookup(TypeNameLookup(i.name.toQualifiedName)).get match {
                case decl: InterfaceDeclaration =>
                  withScope(InterfaceType(decl.fullyQualifiedName.get), Some(env))
                case _ => throw new SyntaxError("Bad fully qualified type.")
              }
            case ci:ClassOrInterfaceType =>
              env.lookup(TypeNameLookup(ci.name.toQualifiedName)).get match {
                case decl: ClassDeclaration =>
                  withScope(ClassType(decl.fullyQualifiedName.get), Some(env))
                case decl: InterfaceDeclaration =>
                  withScope(InterfaceType(decl.fullyQualifiedName.get), Some(env))
                case _ => throw new SyntaxError("Bad fully qualified type.")
              }
            case a:ArrayType =>
              a.subtype match {
                case c:ClassType =>
                  env.lookup(TypeNameLookup(c.name.toQualifiedName)).get match {
                    case decl: ClassDeclaration =>
                      withScope(ArrayType(withScope(ClassType(decl.fullyQualifiedName.get), Some(env))), Some(env))
                    case _ => throw new SyntaxError("Bad fully qualified type.")
                  }
                case i:InterfaceType =>
                  env.lookup(TypeNameLookup(i.name.toQualifiedName)).get match {
                    case decl: InterfaceDeclaration =>
                      withScope(ArrayType(withScope(InterfaceType(decl.fullyQualifiedName.get), Some(env))), Some(env))
                    case _ => throw new SyntaxError("Bad fully qualified type.")
                  }
                case ci:ClassOrInterfaceType =>
                  env.lookup(TypeNameLookup(ci.name.toQualifiedName)).get match {
                    case decl: ClassDeclaration =>
                      withScope(ArrayType(withScope(ClassType(decl.fullyQualifiedName.get), Some(env))), Some(env))
                    case decl: InterfaceDeclaration =>
                      withScope(ArrayType(withScope(InterfaceType(decl.fullyQualifiedName.get), Some(env))), Some(env))
                    case _ => throw new SyntaxError("Bad fully qualified type.")
                  }
                case _ => a
              }
            case _ => argType
          }
          case None => throw new SyntaxError("Could not resolve type for argument \"" + arg + "\"")
        }
        if (result.scope == None) {
          result.scope = arg.scope
        }
        result
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
      case ct: ClassOrInterfaceType => ct.name
      case x =>
        throw new SyntaxError("Can't perform access on non-class, non-interface type: " + x)
    }

    env.lookup(TypeNameLookup(typeName.toQualifiedName)) match {
      case None =>
        throw new SyntaxError("Name " + typeName.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case c: ClassDeclaration => c.body.scope.get
        case i: InterfaceDeclaration => i.body.scope.get
        case _ =>
          throw new SyntaxError("Name " + typeName.niceName + " does not resolve to a class or interface type.")
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

    case (ClassType(TypeName(InputString("String", _, _, _), Some(PackageName(InputString("lang", _, _, _), Some(PackageName(InputString("java", _, _, _), None)))))), _: Type) => true
    case (_: Type, ClassType(TypeName(InputString("String", _, _, _), Some(PackageName(InputString("lang", _, _, _), Some(PackageName(InputString("java", _, _, _), None))))))) => true

    case _ => false
  }

  /**
   * Compare two types. If they are compatible, return the type or the resolve type.
   * Otherwise, throw a type mismatch exception.
   */
  def matchCompatibleType(type1: Option[Type], type2: Option[Type],
                          resolve: Option[Type] = None,
                          compatible:(Type,Type)=>Boolean = compatibleTypes
                      ): Option[Type] = {
    (type1, type2) match {
      case (Some(type1), Some(type2)) if compatible(type1, type2) => {
        if (resolve.isEmpty) {
          Some(type1)
        } else {
          resolve
        }
      }
      case (None, Some(t: ReferenceType)) => Some(t)
      case (Some(t: ReferenceType), None) => Some(t)
      case _ =>
        throw new SyntaxError("Types " + type1 + " and " + type2 + " are not compatible.");
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
      case _ =>
        throw new SyntaxError("Types " + type1 + " and " + type2 + " are not identical.");
    }
  }

  def promotedTypeOption(tpe: Option[Type]): Option[Type] = tpe.map(promotedType(_))

  /**
   * Widening conversions used to promote types.
   * http://www.cs.cornell.edu/andru/javaspec/5.doc.html#170952
   */
  def promotedType(tpe: Type): Type = tpe match {
    // Numeric promotion. Byte, Char, and Short get promoted to Int.
    case ShortKeyword() => IntKeyword()
    case ByteKeyword() => IntKeyword()
    case CharKeyword() => IntKeyword()
    case _ => tpe
  }

  def resolveType(node: AbstractSyntaxNode): Option[Type] = {
    node match {
      case t: Type => Some(t)

      case TrueLiteral() => Some(BooleanKeyword())
      case FalseLiteral() => Some(BooleanKeyword())
      case NullLiteral() => None
      case ThisKeyword() =>
        resolveExpressionName(ExpressionName(InputString("this")), node.scope.get)

      case Num(value, _) => Some(IntKeyword())
      case _: CharLiteral => Some(CharKeyword())
      case _: StringLiteral => Some(withScope(ClassType(QualifiedName("java.lang.String".split("\\.").map(InputString(_))).toTypeName), node.scope))

      case param: FormalParameter => Some(param.varType)
      case field: FieldDeclaration => Some(field.memberType)
      case variable: LocalVariableDeclaration => Some(variable.memberType)
      case variable: ForVariableDeclaration => Some(variable.typeDeclaration)

      case expr: Expression => expr match {
        case c: CastExpression =>
          Some(node.scope.get.asInstanceOf[ScopeEnvironment].fullyQualifyType(c.targetType))
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
          // Binary expression are supported
          case OrExpression(_, _) => Some(BooleanKeyword())
          case AndExpression(_, _) => Some(BooleanKeyword())
          // Eager boolean is supported, bitwise are not
          case BinOrExpression(_, _) => Some(BooleanKeyword())
          case BinXorExpression(_, _) => Some(BooleanKeyword())
          case BinAndExpression(_, _) => Some(BooleanKeyword())
        }

        case relational: RelationalExpression => relational match {
          case EqualExpression(e1, e2)        => Some(BooleanKeyword())
          case NotEqualExpression(e1, e2)     => Some(BooleanKeyword())
          case LessThanExpression(e1, e2)     => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword()))
          case LessEqualExpression(e1, e2)    => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword()))
          case GreaterThanExpression(e1, e2)  => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword()))
          case GreaterEqualExpression(e1, e2) => matchCompatibleType(resolveType(e1), resolveType(e2), Some(BooleanKeyword()))
          case InstanceOfExpression(expr, reference) => {
            (expr, reference) match {
              case (NullLiteral(), _: ReferenceType) => Some(BooleanKeyword())
              case (expr, reference) => (resolveType(expr), reference) match {
                case (Some(_: ReferenceType), _: ReferenceType) => Some(BooleanKeyword())
                case (left, _) => throw new SyntaxError("Left hand side of \"instanceOf\" is not a reference: " + left)
              }
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
        case _: LogicalNotExpression => Some(BooleanKeyword())

        case FieldAccess(ref, name) => {
          val env = node.scope.get
          val scope:Environment = resolvePrimaryAndFindScope(ref, env)
          val fieldName: ExpressionName = ExpressionName(name)

          scope.lookup(ExpressionNameLookup(fieldName.toQualifiedName)) match {
            case None =>
              throw new SyntaxError("Accessing " + fieldName.niceName + " does not resolve.")
            case Some(result) => result match {
              case p: FormalParameter => Some(p.varType)
              case f: FieldDeclaration => Some(f.memberType)
              case v: LocalVariableDeclaration => Some(v.memberType)
              case v: ForVariableDeclaration => Some(v.typeDeclaration)
              case _ => throw new SyntaxError("Accessing non-field!!!")
            }
          }
        }

        case SimpleArrayAccess(name, _) => resolveType(name) match {
          case Some(ArrayType(subtype)) => Some(subtype)
          case _ => throw new SyntaxError("Array access of non-array type expression.")
        }

        case ComplexArrayAccess(ref, expr) => resolveType(ref) match {
          case Some(ArrayType(subtype)) => Some(subtype)
          case _ => throw new SyntaxError("Expression " + ref + " is not an array.")
        }

        case ArrayCreationPrimary(t, _) => Some(ArrayType(t))
        case ClassCreationPrimary(t, _) => Some(t)

        case method: MethodInvocation => method match {
          case SimpleMethodInvocation(ambiguousName, args) => {
            val env = node.scope.get
            resolveMethodName(ambiguousName, args, env) match {
              case Some(method) => Some(method.memberType)
              case None => throw new SyntaxError("Could not resolve method: " + ambiguousName)
            }
          }

          case ComplexMethodInvocation(ref, name, args) => {
            val env = node.scope.get
            val argTypes: Seq[Type] = resolvedTypesForArgs(args, env)
            val scope:Environment = resolvePrimaryAndFindScope(ref, env)

            scope.lookup(MethodLookup(name.toQualifiedName, argTypes)) match {
              case None =>
                throw new SyntaxError("Invoking complex" + name.niceName + " does not resolve.")
              case Some(result) => result match {
                case method: MethodDeclaration => Some(method.memberType)
                case method: InterfaceMemberDeclaration => Some(method.memberType)
                case _ => throw new SyntaxError("Invoking non-method!!!")
              }
            }
          }
        }

        case Assignment(lhs, rhs) => resolveType(lhs)

        case _ => None
      }

      // Some nodes just don't make sense to every look/check for type
      case _ =>  None
    }
  }

  def checkBinaryExpression(e1: Expression, e2: Expression) = {
    resolveType(e1) match {
      case Some(BooleanKeyword()) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
    resolveType(e2) match {
      case Some(BooleanKeyword()) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
  }

  def validateAddExpression(e1: AddExpression) {
    resolveType(e1) match {
      case Some(ClassType(TypeName(InputString("String", _, _, _),
                      Some(PackageName(InputString("lang", _, _, _),
                      Some(PackageName(InputString("java", _, _, _), None))))))) =>
                    Unit
      case Some(t : ReferenceType) => throw new SyntaxError("Can't perform add operations on Reference Types")
      case Some(_) => Unit
      case None => throw new SyntaxError("Can't perform arithmetic operations on Null Types")
    }
  }

  def validateArithmeticExpression(e1: ArithmeticExpression) = {
    resolveType(e1) match {
      case Some(_ : ReferenceType) => throw new SyntaxError("Can't perform arithmetic operations on Refernce Types")
      case Some(_) => Unit
      case None => throw new SyntaxError("Can't perform arithmetic operations on Null Types")
    }
  }

  //  supertype = to, subtype = from - we don't allow implicit upcasting
  def validateSubtypeRelationship(supertype: ReferenceNonArrayType, subtype: ReferenceNonArrayType): Boolean = {
    if (supertype.fullyQualifiedName == CommonNames.JavaLangObject) {
      true
    } else if (supertype.fullyQualified == subtype.fullyQualified) {
      true
    } else {
      supertype.node match {
        case Some(interface: InterfaceDeclaration) => subtype.node match {
          case Some(subclass: ClassDeclaration) => {
            if (subclass.interfaces.map(_.node).contains(interface)) {
              true
            } else {
              subclass.superclass match {
                case Some(subsupertype: ClassType) => validateSubtypeRelationship(supertype, subtype)
                case None => false
              }
            }
          }

          case Some(subinterface: InterfaceDeclaration) =>
            subinterface.interfaces.map(_.node).contains(interface)
        }

        case Some(superclass: ClassDeclaration) => subtype.node match {
          case Some(subclass: ClassDeclaration) => {
            subclass.superclass match {
              case Some(subsuperclass: ClassType) => subsuperclass.node == superclass
              case None => false
            }
          }

          //  Interfaces cannot inherit from classes.
          case Some(subinterface: InterfaceDeclaration) => false
        }

        case None => throw new SyntaxError("Could not resolve supertype: " + supertype)
      }
    }
  }

  def validateTypeConvertability(to: Option[Type], from: Option[Type]) {
    if (to != from) {
      (to, from) match {
        case (Some(ByteKeyword()), Some(CharKeyword())) => throw new SyntaxError("Char type is not assignable to byte type.")
        case (Some(ByteKeyword()), Some(IntKeyword())) => throw new SyntaxError("Int type is not assignable to byte type.")
        case (Some(CharKeyword()), Some(IntKeyword())) => throw new SyntaxError("Int type is not assignable to char type.")
        case (Some(CharKeyword()), Some(ByteKeyword())) => throw new SyntaxError("Byte type is not assignable to char type.")
        case (Some(IntKeyword()), Some(BooleanKeyword())) => throw new SyntaxError("Boolean type is not assignable to int type.")
        case (Some(BooleanKeyword()), Some(IntKeyword())) => throw new SyntaxError("Int type is not assignable to boolean type.")

        //  TODO: Can we *ever* assign from a PrimitiveType array to another PrimitiveType array?
        case (Some(ArrayType(ByteKeyword())), Some(ArrayType(IntKeyword()))) => throw new SyntaxError("Int[] type is not assignable to byte[] type.")
        case (Some(ArrayType(IntKeyword())), Some(ArrayType(ByteKeyword()))) => throw new SyntaxError("Byte[] type is not assignable to int[] type.")

        case (Some(_: PrimitiveType), Some(_: ArrayType)) => throw new SyntaxError("Array type is not assignable to primitive type.")
        case (Some(ArrayType(t)), Some(p: PrimitiveType)) => throw new SyntaxError("Primitive type is not assignable to array type.")
        case (Some(_: PrimitiveType), None) =>
          throw new SyntaxError("Null is not assignable to primitive type.")
        case (_, Some(VoidKeyword())) => throw new SyntaxError("Void return type is not assignable to anything.")

        case (Some(ArrayType(t1)), Some(ArrayType(t2))) => {
          //  TODO: Do hierarchy checking in here to see if the array types are assignable.
          Unit
        }

        // Object arrays are themselves objects, so this is only okay if we're assigning to java.lang.Object.
        case (Some(c: ClassType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
            => throw new SyntaxError("Custom classes cannot be assigned to from arrays.")
        case (Some(c: ClassOrInterfaceType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
            => throw new SyntaxError("Custom classes or interfaces cannot be assigned to from arrays.")
        case (Some(c: InterfaceType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
          => throw new SyntaxError("Custom interfaces cannot be assigned to from arrays.")

        //  Thanks to the case above, this ReferenceType will never be an ArrayType.
        case (Some(ArrayType(_)), Some(_: ReferenceType))
          => throw new SyntaxError("Single object is not assignable to array.")

        case (Some(a: ClassType), Some(b: ClassType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        //  TODO: The following two cases should check the interface hierarchy.
        case (Some(a: ClassType), Some(b: InterfaceType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)
        case (Some(a: InterfaceType), Some(b: ClassType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        case (Some(a: ClassType), Some(b: ClassOrInterfaceType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        case (Some(a: ClassOrInterfaceType), Some(b: ClassType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        case (Some(a: InterfaceType), Some(b: ClassOrInterfaceType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        case (Some(a: ClassOrInterfaceType), Some(b: InterfaceType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

        case _ => Unit
      }
    }
  }

  def validateTypeCastability(to: Option[Type], from: Option[Type]) {
    if (to != from) {
      (to, from) match {
        case (Some(ArrayType(ByteKeyword())), Some(ArrayType(IntKeyword()))) => throw new SyntaxError("Int[] type is not castable to byte[] type.")
        case (Some(ArrayType(t)), Some(p: PrimitiveType)) => throw new SyntaxError("Primitive type is not castable to array type.")

        case (Some(ArrayType(t1)), Some(ArrayType(t2))) => {
          //  TODO: Do hierarchy checking in here to see if the array types are assignable.
          Unit
        }

        // Object arrays are themselves objects, so this is only okay if we're casting to java.lang.Object.
        case (Some(c: ClassType), Some(ArrayType(a))) if c.name.toQualifiedName != CommonNames.JavaLangObject =>
          throw new SyntaxError("Arrays cannot be casted to custom classes.")

        //  Thanks to the case above, this ReferenceType will never be an ArrayType.
        case (Some(ArrayType(_)), Some(c: ClassType)) if c.name.toQualifiedName != CommonNames.JavaLangObject =>
          throw new SyntaxError("Custom classes cannot be casted to arrays.")

        case _ => Unit
      }
    }
  }

  def check(node: AbstractSyntaxNode) {
    val env = node.scope.get
    node match {
      // Check that no bitwise operations occur.
      case BinAndExpression(e1, e2) => TypeChecker.checkBinaryExpression(e1, e2)
      case BinOrExpression(e1, e2) => TypeChecker.checkBinaryExpression(e1, e2)
      case BinXorExpression(e1, e2) => TypeChecker.checkBinaryExpression(e1, e2)

      // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
      case ThisKeyword() => resolveType(node)

      case Assignment(lhs: AbstractSyntaxNode, rhs: AbstractSyntaxNode) =>
        validateTypeConvertability(resolveType(lhs), resolveType(rhs))

      case LocalVariableDeclaration(_, lhs: Type, Some(rhs: Expression)) =>
        validateTypeConvertability(Some(lhs), resolveType(rhs))

      case FieldDeclaration(_, _, lhs: Type, Some(rhs: Expression)) =>
        validateTypeConvertability(Some(lhs), resolveType(rhs))

      case IfStatement(clause: Expression, _, _) if resolveType(clause) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Clause of If statement must be a boolean.")

      case WhileStatement(clause: Expression, _) if resolveType(clause) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Clause of While statement must be a boolean.")

      case CastExpression(lhs: Type, rhs: Expression) =>
        validateTypeCastability(Some(lhs), resolveType(rhs))

      case EqualExpression(e1: Expression, e2: Expression) => {
        try {
          validateTypeConvertability(resolveType(e1), resolveType(e2))
        } catch {
          case _: SyntaxError => {
            validateTypeConvertability(resolveType(e2), resolveType(e1))
          }
        }
      }

      // Check that fields/methods accessed as static are actually static, and that fields/methods
      // accessed as non-static are actually non-static.
      case SimpleMethodInvocation(name, args) => {
        resolveMethodName(name, args, env) match {
          case None => throw new SyntaxError("Could not resolve method: " + name)
          case _ => {}
        }
      }

      case ComplexMethodInvocation(ref, name, args) => {
        val scope : Environment = resolvePrimaryAndFindScope(ref, env)
        resolveMethodName(name, args, scope) match {
          case None => throw new SyntaxError("Could not resolve method: " + name)
          case _ => {}
        }
      }

      case method: MethodDeclaration => {
        val recursiveInvocations = new (AbstractSyntaxNode => Seq[MethodInvocation]) {
          def apply(node: AbstractSyntaxNode): List[MethodInvocation] = {
            node.children.flatMap {
              case k: MethodInvocation => List(k)
              case other => apply(other)
            }
          }
        }

        val invocations: List[AbstractSyntaxNode] = recursiveInvocations(method)
        invocations.foreach {
          case invocation: SimpleMethodInvocation => {
            val env = invocation.scope.get
            resolveMethodName(invocation.name, invocation.args, env) match {
              case None => throw new SyntaxError("Could not resolve method: " + invocation.name)
              case Some(invokedMethod) => {
                val Seq(invokedName, methodName) = Seq(invokedMethod, method).map {
                  case m: MethodDeclaration => m.name.niceName
                  case m: InterfaceMemberDeclaration => m.name.value
                }
              }
            }
          }
          case method: ComplexMethodInvocation => {}
        }
      }

      case LogicalNotExpression(e1) if promotedTypeOption(resolveType(e1)) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Complement operator (!) must be invoked on booleans only.")

      case relational: RelationalExpression => {
        resolveType(relational)
      }

      case arithmetic: ArithmeticExpression => arithmetic match {
        case a: AddExpression => TypeChecker.validateAddExpression(a)
        case e => TypeChecker.validateArithmeticExpression(e)
      }

      case c: ClassCreationPrimary => {
        val className: TypeName = c.classType.name
        val classEnv: Environment = env.lookup(TypeNameLookup(className.toQualifiedName)) match {
          // Check that no objects of abstract classes are created.
          case Some(klass: ClassDeclaration) => {
            if (klass.modifiers.collectFirst{case AbstractKeyword() => true}.isDefined) {
              throw new SyntaxError("Can't create instances of an abstract class.")
            }

            klass.scope.get
          }
          case _ => throw new SyntaxError("Could not find a class declaration: " + className.niceName);
        }

        val types: Seq[Type] = resolvedTypesForArgs(c.args, env)
        val lookup: EnvironmentLookup = ConstructorLookup(QualifiedName(Seq(className.value)), types)
        if (classEnv.lookup(lookup).isEmpty) {
          throw new SyntaxError("Can't find constructor matching arguments: " + className.niceName + "(" + types.mkString(",") + ")")
        }
      }
      case a: ArrayCreationPrimary => {
        resolveType(a.dimExpr) match {
          case  Some(_:NumericType) => Unit
          case _ => throw new SyntaxError("Array index must have numeric type.")
        }
      }
      case f: ForStatement => {
        if (!f.check.isEmpty) {
          resolveType(f.check.get) match {
            case Some(BooleanKeyword()) => Unit
            case e => throw new SyntaxError("Check statement of for loop must be of type bool.")
          }
        }
      }

      case _ => {}
    }
    node.children.foreach { node => check(node) }
  }
}
