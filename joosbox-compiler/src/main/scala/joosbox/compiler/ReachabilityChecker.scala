package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import joosbox.parser._

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object ReachabilityChecker {
  import AbstractSyntaxNode._

  def link(units: Seq[CompilationUnit]): Map[Any, Referenceable] = {
    units.foreach { unit => ReachabilityChecker.check(unit, None) }
    Map.empty
  }

  def resolveConstantValue(node: AbstractSyntaxNode) : Unit = {
    // If it isn't None, that means it has already been constant folded
    // If it is None, it might have been tried and resulted in None, but we don't know for sure
    if (node.constantValue.isEmpty) {
      node match {
        case expr: Expression => {
          node.constantValue = expr match {

            // Direct constant expressions
            case l: Literal => l match {
                case StringLiteral(value) => Some(ConstantString(value.value))
                case CharLiteral(value) => Some(ConstantChar(value.value))
                case TrueLiteral() => Some(ConstantBool("true"))
                case FalseLiteral() => Some(ConstantBool("false"))
                case NullLiteral() => Some(ConstantNull("null"))
                case Num(value, _) => Some(ConstantNum(value))
            }

            // Expressions that need constant folding
            case c: CastExpression => {
              val castType : Option[Type] = TypeChecker.resolveType(c)
              resolveConstantValue(c.expr)
              if (c.expr.constantValue.isEmpty) {
                None
              } else {
                castType match {
                    case Some(_:NumericType)  => Some(ConstantNum(c.expr.constantValue.get.value))
                    case Some(_:BooleanKeyword) => Some(ConstantBool(c.expr.constantValue.get.value))
                    case Some(ClassType(TypeName(InputString("String", _, _, _),
                              Some(PackageName(InputString("lang", _, _, _),
                              Some(PackageName(InputString("java", _, _, _), None))))))) =>
                      Some(ConstantString(c.expr.constantValue.get.value))
                    case _ => None
                }
              }
            }

            case a: ArithmeticExpression => {
              val t : Option[Type] = TypeChecker.resolveType(a)
              resolveConstantValue(a.e1)
              resolveConstantValue(a.e2)
              if (a.e1.constantValue.isEmpty || a.e2.constantValue.isEmpty) {
                None
              } else {
                a match {
                  case AddExpression(_,_) =>
                    t match {
                        case Some(_:NumericType) =>
                          val intValue : Int = a.e1.constantValue.get.value.toInt + a.e2.constantValue.get.value.toInt
                          Some(ConstantNum(intValue.toString))
                        case Some(ClassType(TypeName(InputString("String", _, _, _),
                                  Some(PackageName(InputString("lang", _, _, _),
                                  Some(PackageName(InputString("java", _, _, _), None))))))) =>
                          Some(ConstantString(a.e1.constantValue.get.value + a.e2.constantValue.get.value))
                        case _ => None
                    }
                  case SubtractExpression(_,_) =>
                    t match {
                        case Some(_:NumericType) =>
                          val intValue : Int = a.e1.constantValue.get.value.toInt - a.e2.constantValue.get.value.toInt
                          Some(ConstantNum(intValue.toString))
                        case _ => None
                    }
                  case MultiplyExpression(_,_) =>
                    t match {
                        case Some(_:NumericType) =>
                          val intValue : Int = a.e1.constantValue.get.value.toInt * a.e2.constantValue.get.value.toInt
                          Some(ConstantNum(intValue.toString))
                        case _ => None
                    }
                  case DivideExpression(_,_) =>
                    t match {
                        case Some(_:NumericType) =>
                          val intValue : Int = a.e1.constantValue.get.value.toInt / a.e2.constantValue.get.value.toInt
                          Some(ConstantNum(intValue.toString))
                        case _ => None
                    }
                  case ModExpression(_,_) =>
                    t match {
                        case Some(_:NumericType) =>
                          val intValue : Int = a.e1.constantValue.get.value.toInt % a.e2.constantValue.get.value.toInt
                          Some(ConstantNum(intValue.toString))
                        case _ => None
                    }
                }
              }
            }
            case n: NegatedExpression => {
              val t : Option[Type] = TypeChecker.resolveType(n)
              resolveConstantValue(n.expr)
              if (n.expr.constantValue.isEmpty) {
                None
              } else {
                t match {
                  case Some(_:NumericType) =>
                    val intValue : Int = n.expr.constantValue.get.value.toInt * -1
                    Some(ConstantNum(intValue.toString))
                  case _ => None
                }
              }
            }
            case l: LogicalNotExpression => {
              val t : Option[Type] = TypeChecker.resolveType(l)
              resolveConstantValue(l.expr)
              if (l.expr.constantValue.isEmpty) {
                None
              } else {
                t match {
                  case Some(_:BooleanKeyword) =>
                    val boolValue : Boolean = !(l.expr.constantValue.get.value.toBoolean)
                    Some(ConstantBool(boolValue.toString))
                  case _ => None
                }
              }
            }
            case r: RelationalExpression => {
              r match {
                case EqualExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt == e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case (Some(_:BooleanKeyword), Some(_:BooleanKeyword)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toBoolean == e2.constantValue.get.value.toBoolean)
                        Some(ConstantBool(boolValue.toString))
                      case (Some(ClassType(TypeName(InputString("String", _, _, _),
                                Some(PackageName(InputString("lang", _, _, _),
                                Some(PackageName(InputString("java", _, _, _), None))))))),
                            Some(ClassType(TypeName(InputString("String", _, _, _),
                              Some(PackageName(InputString("lang", _, _, _),
                              Some(PackageName(InputString("java", _, _, _), None)))))))) =>
                        val boolValue : Boolean = (e1.constantValue.get.value == e2.constantValue.get.value)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case NotEqualExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt != e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case (Some(_:BooleanKeyword), Some(_:BooleanKeyword)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toBoolean != e2.constantValue.get.value.toBoolean)
                        Some(ConstantBool(boolValue.toString))
                      case (Some(ClassType(TypeName(InputString("String", _, _, _),
                                Some(PackageName(InputString("lang", _, _, _),
                                Some(PackageName(InputString("java", _, _, _), None))))))),
                            Some(ClassType(TypeName(InputString("String", _, _, _),
                              Some(PackageName(InputString("lang", _, _, _),
                              Some(PackageName(InputString("java", _, _, _), None)))))))) =>
                        val boolValue : Boolean = (e1.constantValue.get.value != e2.constantValue.get.value)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case LessThanExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt < e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case LessEqualExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt <= e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case GreaterThanExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt > e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case GreaterEqualExpression(e1,e2) =>
                  val t1 : Option[Type] = TypeChecker.resolveType(e1)
                  val t2 : Option[Type] = TypeChecker.resolveType(e2)
                  resolveConstantValue(e1)
                  resolveConstantValue(e2)
                  if (e1.constantValue.isEmpty || e2.constantValue.isEmpty) {
                    None
                  } else {
                    (t1, t2) match {
                      case (Some(_:NumericType), Some(_:NumericType)) =>
                        val boolValue : Boolean = (e1.constantValue.get.value.toInt >= e2.constantValue.get.value.toInt)
                        Some(ConstantBool(boolValue.toString))
                      case _ => None
                    }
                  }
                case InstanceOfExpression(e1,t1) => None
              }
            }
            case c : ConditionalExpression => {
              val t1 : Option[Type] = TypeChecker.resolveType(c.e1)
              val t2 : Option[Type] = TypeChecker.resolveType(c.e2)
              resolveConstantValue(c.e1)
              resolveConstantValue(c.e2)
              if (c.e1.constantValue.isEmpty || c.e2.constantValue.isEmpty) {
                None
              } else {
                c match {
                    case _:OrExpression =>
                      val boolValue : Boolean = (c.e1.constantValue.get.value.toBoolean || c.e2.constantValue.get.value.toBoolean)
                      Some(ConstantBool(boolValue.toString))
                    case _:AndExpression =>
                      val boolValue : Boolean = (c.e1.constantValue.get.value.toBoolean && c.e2.constantValue.get.value.toBoolean)
                      Some(ConstantBool(boolValue.toString))
                    case _:BinOrExpression =>
                      val boolValue : Boolean = (c.e1.constantValue.get.value.toBoolean | c.e2.constantValue.get.value.toBoolean)
                      Some(ConstantBool(boolValue.toString))
                    case _:BinAndExpression =>
                      val boolValue : Boolean = (c.e1.constantValue.get.value.toBoolean & c.e2.constantValue.get.value.toBoolean)
                      Some(ConstantBool(boolValue.toString))
                    case _:BinXorExpression =>
                      val boolValue : Boolean = (c.e1.constantValue.get.value.toBoolean ^ c.e2.constantValue.get.value.toBoolean)
                      Some(ConstantBool(boolValue.toString))
                }
              }
            }
            /* TODO:
             *  Simple names that refer to final variables whose initializers are constant expressions
             *
             *  Qualified names of the form TypeName . Identifier that refer to final variables whose initializers are
             *  constant expressions
             *
             */
            case _ => None
          }
        }
        case _ => None
      }
    }
  }

  def check(node: AbstractSyntaxNode, parent: Option[AbstractSyntaxNode]) : Unit = {
    def takeAfter[A](elem: A, seq:Seq[A]) : Seq[A] = {
      seq.slice(seq.indexOf(elem), seq.indexOf(seq.last))
    }

    var siblings : Seq[AbstractSyntaxNode] = Seq.empty[AbstractSyntaxNode]
    if (!parent.isEmpty) {
      siblings = takeAfter(node, parent.get.children)
    }

    node match {
        case w : WhileStatement =>
          resolveConstantValue(w.clause)
          w.clause.constantValue match {
              case Some(ConstantBool("false")) => throw new SyntaxError("Unreachable body of while-loop statement.")
              case Some(ConstantBool("true")) =>
                if (!siblings.isEmpty) {
                  throw new SyntaxError("Unreachable statements after while-loop statement.")
                }
              case _ => Unit
          }
        case f : ForStatement =>
          if (!f.check.isEmpty) {
            resolveConstantValue(f.check.get)
            f.check.get.constantValue match {
                case Some(ConstantBool("false")) => throw new SyntaxError("Unreachable body of for-loop statement.")
                case _ => Unit
            }
          }
        case _ => Unit
    }

    node.children.foreach { child => check(child, Some(node)) }
  }
}
