package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object TypeChecker {
  import AbstractSyntaxNode._

  def link(
    units: Seq[CompilationUnit],
    mapping: EnvironmentMapping
  ): Map[Any, Referenceable] = {
    units.foreach { unit =>
      TypeChecker.check(unit)(mapping)
    }
    Map.empty
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

  def resolveType(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping): Option[Type] = {
    node match {
      case t: Type => Some(t)

      case TrueLiteral => Some(BooleanKeyword)
      case FalseLiteral => Some(BooleanKeyword)
      case ThisKeyword => None // TODO
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
            val env = mapping.enclosingScopeOf(node).get
            env.lookup(ExpressionNameLookup(name)) match {
              case None => throw new SyntaxError("Name " + name.niceName + " does not resolve to a type.")
              case Some(result) => result match {
                case p: FormalParameter => Some(p.varType)
                case f: FieldDeclaration => Some(f.memberType)
                case v: LocalVariableDeclaration => Some(v.memberType)
                case v: ForVariableDeclaration => Some(v.typeDeclaration)
                case _ => throw new SyntaxError("Could not resolve type of expression.");
              }
            }
          }
          case _ => None
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

        //TODO
        case FieldAccess(ref, name) => None
        case SimpleArrayAccess(name, expr) => None
        case ComplexArrayAccess(ref, expr) => None

        case ArrayCreationPrimary(t, expr) => None
        case ClassCreationPrimary(t, args) => None

        case method : MethodInvocation => method match {
          case SimpleMethodInvocation(name, args) => None
          case ComplexMethodInvocation(ref, name, args) => None
        }

        case _ => None
      }

      case statement: Statement => statement match {
        case ReturnStatement(expr) =>
          if (expr.isEmpty) {
            Some(VoidKeyword)
          } else {
            resolveType(expr.get)
          }
        // Loops, ifs, etc. don't make sense to check for type 
        case _ => None
      }

      // Some nodes just don't make sense to every look/check for type
      case _ => None
    }
  }

  def checkLogicalExpression(e1: Expression, e2: Expression)(implicit mapping: EnvironmentMapping) = {
    resolveType(e1) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
    resolveType(e2) match {
      case Some(BooleanKeyword) => Unit
      case _ => throw new SyntaxError("Bitwise operators aren't supported.")
    }
  }

  def check(node: AbstractSyntaxNode)(implicit mapping: EnvironmentMapping) {
    val env = mapping.enclosingScopeOf(node).get
    node match {
      case e: AndExpression =>
        Unit //TypeChecker.checkLogicalExpression(e.e1, e.e2)
      case e: OrExpression =>
        Unit //TypeChecker.checkLogicalExpression(e.e1, e.e2)
      /*
      case c: ClassCreationPrimary => {
        val className: TypeName = c.classType.name
        env.lookup(TypeNameLookup(className)) match {
          case Some(klass: ClassDeclaration) => {
            if (klass.modifiers.contains(AbstractKeyword)) {
              throw new SyntaxError("Can't create instances of an abstract class.")
            }
          }
          case _ => {}
        }

        // Check to see if the constructor exists by searching for a constructor
        // with the correct signature.
        val types: Seq[Type] = c.args.map { arg =>
          resolveType(arg) match {
            case Some(argType) => argType
            case None => throw new SyntaxError("Could not resolve type for argument \"" + arg + "\"")
          }
        }

        val lookup: EnvironmentLookup = ConstructorLookup(className.value, types)
        if (env.lookup(lookup).isEmpty) {
          throw new SyntaxError("Can't find constructor matching arguments: " + className.niceName + "(" + types.mkString(",") + ")")
        }
      }
      */
      case _ => {}
    }
    node.children.foreach { node => check(node) }
  }
}
