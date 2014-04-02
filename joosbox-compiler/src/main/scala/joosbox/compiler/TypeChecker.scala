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
        case Some(ClassOrInterfaceType(n)) => {
          val l = TypeNameLookup(n.toQualifiedName)
          val result = env.lookup(l)
          result.get.scope.get
        }

        case _ => throw new SyntaxError("Couldn't resolve type " + typeOption + ".")
      }
    } getOrElse(env)

    val argTypes: Seq[Type] = resolvedTypesForArgs(args, updatedEnv)
    val lookup = MethodLookup(QualifiedName(Seq(name.value)), argTypes)

    updatedEnv.lookup(lookup) match {
      case Some(result) => result match {
        case method: TypeMethodDeclaration => if (method.isStatic) {
          name.prefix match {
            case Some(_: ExpressionName) => throw new SyntaxError("Cannot access static method from instance context: " + name)
            case _ => Some(method)
          }
        } else {
          name.prefix match {
            case Some(_: TypeName) => throw new SyntaxError("Cannot access instance method from static context: " + name)
            case _ => Some(method)
          }
        }
        case _ =>
          throw new SyntaxError("Found non-method " + result)
      }
      case _ => None
    }
  }

  def withScope[T <: AbstractSyntaxNode](t: T, s: Option[Environment]): T = {
    if (t.scope == None) {
      t.scope = s
    }
    t
  }

  def resolveTypeName(name: TypeName, env: Environment): Option[ReferenceNonArrayType] = {
    env.lookup(TypeNameLookup(name.toQualifiedName)) match {
      case None => throw new SyntaxError("TypeName " + name.niceName + " does not resolve to a type.")
      case Some(result) => result match {
        case _: ClassDeclaration => Some(withScope(ClassType(name), name.scope))
        case _: InterfaceDeclaration => Some(withScope(InterfaceType(name), name.scope))
        case _ => throw new SyntaxError("Name " + name.niceName + " does not resolve to a class or interface type.")
      }
    }
  }

  /**
   * Resolves an expression name to a field declaration, and it's prefix's Type Declaration (class or interface)
   * If returns None, then the expression did not point to a field declaration.
   */
  def resolveFieldDeclaration(name: ExpressionName, env: Environment): Option[(FieldDeclaration, TypeDeclaration)] = {
    val result: Option[(Option[TypeDeclaration], Environment)] = name.prefix match {
      case Some(prefix) =>
        val prefixType: Option[Type] = prefix match {
          case n: ExpressionName => resolveExpressionName(n, env)
          case other => resolveType(other)
        }

        prefixType match {
          case Some(result) => {
            val name = result match {
              case x: ClassOrInterfaceType => Some(x.name.toQualifiedName)
              case x: ClassType => Some(x.name.toQualifiedName)
              case x: InterfaceType => Some(x.name.toQualifiedName)
              case _ => None
            }

            name match {
              case Some(name) => env.lookup(TypeNameLookup(name)) match {
                case Some(cd: ClassDeclaration) =>
                  Some(Some(cd), cd.body.scope.get.parent.get)
                case Some(id: InterfaceDeclaration) => Some(Some(id), id.body.scope.get.parent.get)
                case _ => throw new SyntaxError("Invalid declaration.")
              }
              case _ => None
            }
          }
          case _ => None
        }
      case _ => Some(None, env)
    }

    result match {
      case Some((Some(decl), prefixScope)) =>
        val lookup = ExpressionNameLookup(ExpressionName(name.value).toQualifiedName)
        prefixScope.lookup(lookup) match {
          case Some(result) => result match {
            case field: FieldDeclaration => Some((field, decl))
            case _ => None
          }
          case _ => throw new SyntaxError("ExpressionName " + name + " does not resolve to a type.")
        }
      case Some((None, _)) => None
      case _ => None
    }
  }

  def resolveExpressionName(name: ExpressionName, env: Environment): Option[Type] = {
    var tmpEnv = env
    if (!name.prefix.isEmpty) {
      val t: Option[Type] = name.prefix.get match {
        case n: ExpressionName => resolveExpressionName(n, env)
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
              //  Length is a final field, so it writeable must be set to false.
              val intType = IntKeyword()
              intType.writeable = false
              return Some(intType)
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
        case f: FieldDeclaration => if (f.isStatic) {
          name.prefix match {
            case Some(_: ExpressionName) => throw new SyntaxError("Cannot access static field from instance context: " + name)
            case _ => Some(f.memberType)
          }
        } else {
          name.prefix match {
            case Some(_: TypeName) => throw new SyntaxError("Cannot access instance field from static context: " + name)
            case _ => Some(f.memberType)
          }
        }
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

  def resolvePrimary(ref:Primary, env:Environment) : Type = {
    val primaryType: Option[Type] = resolveType(ref)
    if (primaryType.isEmpty) {
      throw new SyntaxError("Attempted access of unresolvable type.")
    }
    primaryType.get
  }

  def resolvePrimaryAndFindScope(ref:Primary, env:Environment) : Environment = {
    val typeName: TypeName = resolvePrimary(ref, env) match {
      case c: ClassType => c.name
      case i: InterfaceType => i.name
      case ct: ClassOrInterfaceType => ct.name
      case at: ArrayType => CommonNames.JavaLangObject.toTypeName
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

  def checkFieldProtection(name: ExpressionName, expressionType: TypeDeclaration, field: FieldDeclaration)(env: Environment) {
    if (!field.isProtected) {
      return
    }

    val scope: Environment = field.scope.get
    val (thatPackage: PackageDeclaration, thatType: TypeDeclaration) = scope.compilationScope.get.node match {
      case Some(c: CompilationUnit) => (c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage), c.typeDeclaration)
      case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
    }

    val (thisPackage: PackageDeclaration, thisType: TypeDeclaration) = env.compilationScope.get.node match {
      case Some(c: CompilationUnit) => (c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage), c.typeDeclaration)
      case _ => throw new Exception("Method invocation context has no compilation unit.")
    }

    if (thatPackage == thisPackage) {
      return
    }

    checkProtectionAccess(thatType, thisType, expressionType, Right(field), field.isStatic)
  }

  def checkClassCreationProtection(creation: ClassCreationPrimary)(env: Environment) {
    val argTypes = resolvedTypesForArgs(creation.args, env)
    val lookup: EnvironmentLookup = ConstructorLookup(QualifiedName(Seq(creation.classType.name.value)), argTypes)
    val constructor: ConstructorDeclaration = env.lookup(lookup) match {
      case Some(x: ConstructorDeclaration) => x
      case _ => throw new Exception("Couldn't resolve constructor.")
    }

    if (!constructor.isProtected) {
      return
    }

    val scope: Environment = constructor.scope.get
    val thatPackage: PackageDeclaration = scope.compilationScope.get.node match {
      case Some(c: CompilationUnit) => c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage)
      case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
    }

    val thisPackage: PackageDeclaration = env.compilationScope.get.node match {
      case Some(c: CompilationUnit) => c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage)
      case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
    }

    if (thatPackage != thisPackage) {
      throw new SyntaxError("Cannot call protected constructor from a different package.")
    }
  }

  def checkMethodProtection(name: MethodName, method: TypeMethodDeclaration)(env: Environment) {
    if (!method.isProtected) {
      return
    }

    val scope: Environment = method.scope.get
    val (thatPackage: PackageDeclaration, thatType: TypeDeclaration) = scope.compilationScope.get.node match {
      case Some(c: CompilationUnit) => (c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage), c.typeDeclaration)
      case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
    }

    val (thisPackage: PackageDeclaration, thisType: TypeDeclaration) = env.compilationScope.get.node match {
      case Some(c: CompilationUnit) => (c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage), c.typeDeclaration)
      case _ => throw new Exception("Method invocation context has no compilation unit.")
    }

    if (thatPackage == thisPackage) {
      return
    }

    val expressionType: TypeDeclaration = name.prefix match {
      case Some(prefix) => {
        val namePrefix = NameLinker.disambiguateName(prefix)(env)
        resolveType(namePrefix) match {
          case None => throw new Exception("Could not resolve prefix of method name: " + name)
          case Some(tpe) => {
            val typeName = tpe match {
              case ClassType(name) => name
              case InterfaceType(name) => name
              case _ => throw new Exception("Not a class or interface type.");
            }

            env.lookup(TypeNameLookup(typeName.toQualifiedName)) match {
              case Some(x: TypeDeclaration) => x
              case _ => throw new Exception("Could not find type declaration for " + typeName)
            }
          }
        }
      }
      case _ => thisType
    }

    checkProtectionAccess(thatType, thisType, expressionType, Left(method), method.isStatic)
  }

  def checkProtectionAccess(
    thatType: TypeDeclaration,
    thisType: TypeDeclaration,
    expressionType: TypeDeclaration,
    methodOrField: Either[TypeMethodDeclaration, FieldDeclaration],
    static: Boolean
  ) = {
    val (niceType, niceName) = methodOrField match {
      case Left(method) => ("method", method.niceName)
      case Right(field) => ("field", field.name.niceName)
    }

    (thatType, thisType, expressionType) match {
      case (thatClass: ClassDeclaration, thisClass: ClassDeclaration, expressionClass: ClassDeclaration) => {
        if (static) {
          // In static methods, expressionClass is identical to thatClass.
          // If the class that is invoked is a subclass, then
          if (thatClass.isSameOrSubclassOf(thisClass)) {
            throw new SyntaxError(s"Static protected ${niceType} ${niceName} defined " +
              s"in ${thatType.name.niceName} invoked from class " +
              s"${thisType.name.niceName} is defined in a subclass.")
          }
        } else {
          // When a method is invoked on an object, we check if the caller's class (thisClass) is
          // Access is permitted if and only if the type of E is S or a subclass of S.
          if (expressionClass.isSameOrSubclassOf(thisClass)) {
            if (thatClass.isSameOrSubclassOf(expressionClass)) {
              throw new SyntaxError(s"Protected ${niceType} ${niceName} defined " +
                s"in ${thatType.name.niceName} invoked from class " +
                s"${thisType.name.niceName} is defined in a subclass.")
            }
          } else {
            throw new SyntaxError(s"Protected ${niceType} ${niceName} defined " +
              s"invoked on ${expressionType.name.niceName} from class " +
              s"${thisType.name.niceName} is not a subclass.")
          }
        }
      }
      case _ => {}
    }
  }

  def compatibleTypes(type1: Type, type2: Type): Boolean = (type1, type2) match {
    // Exact types are compatible.
    case (one, two) if one == two => true
    // Reference types are compatible.
    case (one: ReferenceType, two: ReferenceType) => true
    // Numeric types are always compatible with each other.
    case (one: NumericType, two: NumericType) => true

    case (ClassType(n), y: Type)
      if n.toQualifiedName == CommonNames.JavaLangString
        && y != VoidKeyword()  => true
    case (y: Type, ClassType(n))
      if n.toQualifiedName == CommonNames.JavaLangString
        && y != VoidKeyword() => true

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
      case (Some(type1: ClassType), Some(type2))
        if compatible(type1, type2) && type1.name.toQualifiedName == CommonNames.JavaLangString
          => Some(type1)

      case (Some(type1), Some(type2: ClassType))
        if compatible(type1, type2) && type2.name.toQualifiedName == CommonNames.JavaLangString
          => Some(type2)

      case (Some(type1), Some(type2)) if compatible(type1, type2) => {
        resolve orElse Some(type1)
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
    val result = node match {
      case t: Type => Some(t)

      case TrueLiteral() => Some(BooleanKeyword())
      case FalseLiteral() => Some(BooleanKeyword())
      case NullLiteral() => None
      case ThisKeyword() =>
        resolveExpressionName(withScope(ExpressionName(InputString("this")), node.scope), node.scope.get)

      case Num(value, _) => Some(IntKeyword())
      case _: CharLiteral => Some(CharKeyword())
      case _: StringLiteral => Some(withScope(ClassType(CommonNames.JavaLangString.toTypeName), node.scope))

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
          val refType : Type = resolvePrimary(ref, env)
          val scope:Environment = resolvePrimaryAndFindScope(ref, env)
          val fieldName: ExpressionName = ExpressionName(name)

          // Check validity of field access for arrays
          refType match {
            case a : ArrayType => 
              if (fieldName.value == InputString("length")) {
                return Some(IntKeyword())
              } else {
                throw new SyntaxError("Only valid array field is length.")
              }
            case _ => Unit 
          }

          resolveExpressionName(fieldName, scope)
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

              //  We can't call a static method without naming the class.
              case Some(method) if method.isStatic && ambiguousName.prefix.isEmpty =>
                throw new SyntaxError("Static method cannot be called without class name: " + ambiguousName)

              case Some(method) => method.memberType match {
                case r: ReferenceNonArrayType => resolveTypeName(r.name, r.scope.get)
                case ArrayType(r: ReferenceNonArrayType) => Some(withScope(ArrayType(resolveTypeName(r.name, r.scope.get).get), r.scope))
                case ArrayType(_: PrimitiveType) => Some(method.memberType)
                case p: PrimitiveType => Some(p)
                case VoidKeyword() => Some(VoidKeyword())
                case _ =>
                  throw new SyntaxError("Could not match type of simple method invocation: " + node)
              }
              case None =>
                throw new SyntaxError("Could not resolve method: " + ambiguousName)
            }
          }

          case ComplexMethodInvocation(ref, name, args) => {
            val env = node.scope.get
            val argTypes: Seq[Type] = resolvedTypesForArgs(args, env)
            val scope:Environment = resolvePrimaryAndFindScope(ref, env)

            scope.lookup(MethodLookup(name.toQualifiedName, argTypes)) match {
              case None =>
                throw new SyntaxError("Invoking complex method " + name + " does not resolve.")
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

    //  Fully qualify everything we can.
    result match {
      case Some(c: ClassType) => Some(withScope(c.fullyQualified, c.scope))
      case Some(c: InterfaceType) => Some(withScope(c.fullyQualified, c.scope))
      case Some(c: ClassOrInterfaceType) => Some(withScope(c.fullyQualified, c.scope))
      case _ => result
    }
  }

  def checkBooleanExpression(e1: Expression, e2: Expression) = {
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
    val t : Option[Type] = resolveType(e1)
    t match {
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
            if (subclass.interfaces.flatMap(_.node).contains(interface)) {
              true

              //  Do any of our interfaces implement the supertype?
            } else if (subclass.interfaces.toStream.map(validateSubtypeRelationship(supertype, _)).collectFirst{ case true => true }.getOrElse(false)) {
              true
            } else {
              subclass.superclass match {
                case Some(subsupertype: ClassType) => validateSubtypeRelationship(supertype, subtype)
                case None
                  => false
              }
            }
          }

          case Some(subinterface: InterfaceDeclaration) =>
            subinterface.interfaces.flatMap(_.node).contains(interface)
        }

        case Some(superclass: ClassDeclaration) => subtype.node match {
          case Some(subclass: ClassDeclaration) => {
            subclass.superclass match {
              case Some(subsuperclass: ClassType) => subsuperclass.node match {
                case Some(x) => x == superclass
                case None => false
              }
              case None
                => false
            }
          }

          //  Interfaces cannot inherit from classes.
          case Some(subinterface: InterfaceDeclaration)
            => false
        }

        case None => throw new SyntaxError("Could not resolve supertype: " + supertype)
      }
    }
  }

  def validateTypeComparability(to: Option[Type], from: Option[Type]) {
    if (to == Some(VoidKeyword()) && from == Some(VoidKeyword())) {
      throw new SyntaxError("Void types cannot be compared.")
    } else {
      validateTypeConvertability(to, from)
    }
  }

  def validateTypeConvertability(to: Option[Type], from: Option[Type]) {
    if (to != from) {
      (to, from) match {
        case (Some(VoidKeyword()), Some(x))
          => throw new SyntaxError("Type cannot be converted to void: " + x)

        case (Some(VoidKeyword()), None)
          => throw new SyntaxError("Type cannot be converted to void from statement with no type.")

        case (Some(_: ShortKeyword), Some(_: ByteKeyword)) => Unit
        case (Some(_: ByteKeyword), Some(_: ShortKeyword)) => Unit
        case (Some(_: IntKeyword), Some(_: ByteKeyword)) => Unit
        case (Some(_: IntKeyword), Some(_: CharKeyword)) => Unit
        case (Some(_: IntKeyword), Some(_: ShortKeyword)) => Unit


        //  Except for the cases above, primitives are not convertible.
        case (Some(_: PrimitiveType), Some(_: PrimitiveType)) => throw new SyntaxError("Types not convertible: " + to + " from " + from)

        case (Some(c: ReferenceType), Some(x: PrimitiveType))
          => throw new SyntaxError("Primitive type " + x + " is not convertible to reference type.")

        //  TODO: Can we *ever* assign from a PrimitiveType array to another PrimitiveType array?
        case (Some(ArrayType(ByteKeyword())), Some(ArrayType(IntKeyword()))) => throw new SyntaxError("Int[] type is not convertible to byte[] type.")
        case (Some(ArrayType(IntKeyword())), Some(ArrayType(ByteKeyword()))) => throw new SyntaxError("Byte[] type is not convertible to int[] type.")

        case (Some(_: PrimitiveType), Some(_: ArrayType)) => throw new SyntaxError("Array type is not convertible to primitive type.")
        case (Some(_: PrimitiveType), None) => throw new SyntaxError("Null is not convertible to primitive type.")
        case (_, Some(VoidKeyword())) => throw new SyntaxError("Void return type is not convertible to anything.")

        //  Except for the cases above, primitives are not convertible.
        case (Some(_: PrimitiveType), Some(_: ReferenceType)) => throw new SyntaxError("Reference type is not convertible to primitive: " + from + " from " + to)

        case (Some(ArrayType(t1)), Some(ArrayType(t2))) => {
          //  TODO: Do hierarchy checking in here to see if the array types are assignable.
          Unit
        }

        // Object arrays are themselves objects, so this is only okay if we're assigning to java.lang.Object.
        case (Some(c: ClassType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
            => throw new SyntaxError("Custom classes cannot be converted to from arrays.")
        case (Some(c: ClassOrInterfaceType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
            => throw new SyntaxError("Custom classes or interfaces cannot be converted to from arrays.")
        case (Some(c: InterfaceType), Some(ArrayType(a))) if !CommonNames.doesAcceptArrayAssignment(c)
          => throw new SyntaxError("Custom interfaces cannot be converted to from arrays.")

        //  Thanks to the case above, this ReferenceType will never be an ArrayType.
        case (Some(ArrayType(_)), Some(_: ReferenceType))
          => throw new SyntaxError("Single object is not assignable to array.")

        case (Some(a: ClassType), Some(b: ClassType)) if !validateSubtypeRelationship(a, b)
          => throw new SyntaxError("Types are not convertible: " + a.fullyQualified + " and " + b.fullyQualified)

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

        case x
          => Unit
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

  def validateReturnStatementType(s: BlockStatement, t: Type): Unit = {
    s match {
      case Block(statements) => statements.foreach {
        case bs: BlockStatement => validateReturnStatementType(bs, t)
      }
      case ReturnStatement(None) =>
        if (t != VoidKeyword()) {
          throw new SyntaxError("Invalid return type for method; got void expected non-void.")
        }
      case ReturnStatement(Some(e: Expression)) =>
        validateTypeConvertability(Some(t), resolveType(e))
      case _ => Unit
    }
  }

  def check(node: AbstractSyntaxNode) {
    val env = node.scope match {
      case Some(x) => x
      case _ => throw new SyntaxError("Node has no scope: " + node)
    }

    node match {

      // Check that no bitwise operations occur.
      case AndExpression(e1, e2) => TypeChecker.checkBooleanExpression(e1, e2)
      case OrExpression(e1, e2) => TypeChecker.checkBooleanExpression(e1, e2)

      // Check that no bitwise operations occur.
      case BinAndExpression(e1, e2) => TypeChecker.checkBooleanExpression(e1, e2)
      case BinOrExpression(e1, e2) => TypeChecker.checkBooleanExpression(e1, e2)
      case BinXorExpression(e1, e2) => TypeChecker.checkBooleanExpression(e1, e2)

      // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
      case ThisKeyword() => resolveType(node)

      case Assignment(lhs: AbstractSyntaxNode, rhs: AbstractSyntaxNode) => {
        val lType = resolveType(lhs)
        lType match {
          case Some(t: Type) if !t.writeable => throw new SyntaxError("Cannot assign to final field: " + lhs)
          case _ => Unit
        }
        validateTypeConvertability(lType, resolveType(rhs))
      }

      case LocalVariableDeclaration(_, lhs: Type, rhs: Expression) =>
        validateTypeConvertability(Some(lhs), resolveType(rhs))

      case FieldDeclaration(_, _, lhs: Type, Some(rhs: Expression)) =>
        validateTypeConvertability(Some(lhs), resolveType(rhs))

      case IfStatement(clause: Expression, _, _) if resolveType(clause) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Clause of If statement must be a boolean.")

      case WhileStatement(clause: Expression, _) if resolveType(clause) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Clause of While statement must be a boolean.")

      case ReturnStatement(Some(e: Expression))  => resolveType(e) match {
         case Some(VoidKeyword()) => throw new SyntaxError("Cannot return something of type void: " + e)
         case _ => Unit
      }

      case CastExpression(lhs: Type, rhs: Expression) =>
        validateTypeCastability(Some(lhs), resolveType(rhs))

      case EqualExpression(e1: Expression, e2: Expression) => {
        try {
          validateTypeComparability(resolveType(e1), resolveType(e2))
        } catch {
          case _: SyntaxError => {
            validateTypeComparability(resolveType(e2), resolveType(e1))
          }
        }
      }

      case name: ExpressionName => {
        val disambiguated = NameLinker.disambiguateName(name)(env).asInstanceOf[ExpressionName]
        resolveFieldDeclaration(disambiguated, env) match {
          case Some((field, expressionType)) => checkFieldProtection(name, expressionType, field)(env)
          case _ => {}
        }
      }

      // Check that fields/methods accessed as static are actually static, and that fields/methods
      // accessed as non-static are actually non-static.
      case SimpleMethodInvocation(name, args) => {
        resolveMethodName(name, args, env) match {
          case Some(method: TypeMethodDeclaration) => checkMethodProtection(name, method)(env)
          case _ => throw new SyntaxError("Could not resolve method: " + name)
        }
      }

      case ComplexMethodInvocation(ref, name, args) => {
        val scope: Environment = resolvePrimaryAndFindScope(ref, env)
        resolveMethodName(name, args, scope) match {
          case None => throw new SyntaxError("Could not resolve method: " + name)
          case _ => {}
        }
      }

      // Validate that no constructor returns anything, and that the super constructor
      // with no arguments always exists.
      case ConstructorDeclaration(name: MethodName, _, _, Some(b: Block)) => {
        b.statements.lastOption match {
          case Some(b: BlockStatement) => validateReturnStatementType(b, VoidKeyword())
          case _ => Unit
        }

        node.scope.get.getEnclosingClassNode match {
          case Some(c: ClassDeclaration) => c.superclass match {
            case Some(st: ClassType) => st.node match {
              case Some(superclass: ClassDeclaration) => {
                val lookup = ConstructorLookup(superclass.name.toQualifiedName, Seq.empty[Type])
                superclass.scope.get.lookup(lookup) match {
                  case None => throw new SyntaxError("No argument-free super constructor found for class: " + c.name.niceName)
                  // A super constructor exists with no arguments. We good.
                  case _ => {}
                }
              }
              case _ => throw new SyntaxError("Could not find superclass node from constructor: " + node)
            }
            case _ => {}
          }
          case _ => throw new SyntaxError("Could not find ClassDeclaration from constructor to validate super constructor: " + node)
        }
      }

      case method: MethodDeclaration => {
        val returnStatementSearch = new (AbstractSyntaxNode => List[ReturnStatement]) {
          def apply(node: AbstractSyntaxNode): List[ReturnStatement] = {
            node.children.flatMap {
              case k: ReturnStatement => List(k)
              case other => apply(other)
            }
          }
        }

        // Check to see if method returns the proper type.
        returnStatementSearch(method).foreach(validateReturnStatementType(_, method.memberType))

        val expressionNameSearch = new (AbstractSyntaxNode => List[ExpressionName]) {
          def apply(node: AbstractSyntaxNode): List[ExpressionName] = {
            node.children.flatMap {
              case k: ExpressionName => List(k)
              case other => apply(other)
            }
          }
        }

        val expressionNames = expressionNameSearch(method)
        expressionNames.foreach { name => name match {
          case ExpressionName(_, None) => {
            val lookup = ExpressionNameLookup(name.toQualifiedName)
            name.scope.get.lookup(lookup) match {
              case None => throw new SyntaxError("ExpressionName " + name + " does not resolve to a type.")
              case Some(result) => result match {
                case f: FieldDeclaration => {
                  if (method.isStatic && !f.isStatic) {
                    throw new SyntaxError("Non-static field " + f.name + " being referenced in static method " + method.niceName)
                  } else if (!method.isStatic && f.isStatic) {
                    throw new SyntaxError("Static field " + f.name + " being referenced in non-static method " + method.niceName)
                  }
                }
                case _ => {}
              }
            }
          }
          case _ => {}
        }}

        // Search for invalid method invocations.
        val methodInvocationSearch = new (AbstractSyntaxNode => List[MethodInvocation]) {
          def apply(node: AbstractSyntaxNode): List[MethodInvocation] = {
            node.children.flatMap {
              case k: MethodInvocation => List(k)
              case other => apply(other)
            }
          }
        }

        val invocations: List[MethodInvocation] = methodInvocationSearch(node)
        invocations.foreach {
          case invocation: ComplexMethodInvocation => resolvePrimary(invocation.primary, env)
          case invocation: SimpleMethodInvocation => {
            val env = invocation.scope.get

            // If the invocation is implicitly on this class (ie. staticMethod()), then check to see if the
            // method is also static.
            if (invocation.name.prefix.isEmpty) {
              resolveMethodName(invocation.name, invocation.args, env) match {
                case None =>
                  throw new SyntaxError("Could not resolve method: " + invocation.name)
                case Some(invokedMethod: TypeMethodDeclaration) => {
                  if (method.isStatic && !invokedMethod.isStatic) {
                    throw new SyntaxError("Non-static method " + invokedMethod.niceName +
                      " called within static method " + method.niceName)
                  } else if (!method.isStatic && invokedMethod.isStatic) {
                    throw new SyntaxError("Static method " + invokedMethod.niceName +
                      " called within non-static method " + method.niceName)
                  }
                }
              }
            } else {
              resolveMethodName(invocation.name, invocation.args, env) match {
                case None =>
                  throw new SyntaxError("Could not resolve method: " + invocation.name)
                case Some(invokedMethod: TypeMethodDeclaration) => {
                }
              }
            }
          }
        }
      }

      case LogicalNotExpression(e1) if promotedTypeOption(resolveType(e1)) != Some(BooleanKeyword()) =>
        throw new SyntaxError("Complement operator (!) must be invoked on booleans only.")

      case relational: RelationalExpression => resolveType(relational)

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

        val constructor = classEnv.lookup(lookup) match {
          case Some(constructor: ConstructorDeclaration) => constructor
          case Some(x) => throw new Exception("Didn't get a constructor declaration back. Wat. " + x)
          case None => throw new SyntaxError("Can't find constructor matching arguments: " + className.niceName + "(" + types.mkString(",") + ")")
        }

        //
        // Protected verification.
        //

        if (!constructor.isProtected) {
          return
        }

        val scope: Environment = constructor.scope.get
        val thatPackage: PackageDeclaration = scope.compilationScope.get.node match {
          case Some(c: CompilationUnit) => c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage)
          case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
        }

        val thisPackage: PackageDeclaration = env.compilationScope.get.node match {
          case Some(c: CompilationUnit) => c.packageDeclaration.getOrElse(PackageDeclaration.implicitPackage)
          case _ => throw new Exception("Reference in complex method invocation has no compilation unit.")
        }

        if (thatPackage != thisPackage) {
          throw new SyntaxError("Cannot call protected constructor from a different package.")
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

