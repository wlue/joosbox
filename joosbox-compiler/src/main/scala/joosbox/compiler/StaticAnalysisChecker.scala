package joosbox.compiler

import joosbox.parser.AbstractSyntaxNode
import joosbox.lexer.InputString
import joosbox.lexer.SyntaxError

import joosbox.parser._

import AbstractSyntaxNode.CompilationUnit
import AbstractSyntaxNode.Referenceable


object StaticAnalysisChecker {
  import AbstractSyntaxNode._

  def link(units: Seq[CompilationUnit]): Map[Any, Referenceable] = {
    units.foreach { unit => StaticAnalysisChecker.check(unit, None) }
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
            case e: ParenthesizedExpression => {
              resolveConstantValue(e.expr)
              e.expr.constantValue
            }
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
                          if (a.e2.constantValue.get.value.toInt == 0) {
                            None
                          } else {
                            val intValue : Int = a.e1.constantValue.get.value.toInt / a.e2.constantValue.get.value.toInt
                            Some(ConstantNum(intValue.toString))
                          }
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
            case e : ExpressionName => {
              var env : Environment = e.scope.get
              if (!e.prefix.isEmpty) {
                val name : ExpressionName = NameLinker.disambiguateName(e)(env).asInstanceOf[ExpressionName]
                val t:Option[Type] = name.prefix.get match {
                  case n:ExpressionName => TypeChecker.resolveExpressionName(n, env)
                  case other => TypeChecker.resolveType(other)
                }
                t match {
                  case Some(x:ClassOrInterfaceType) => {
                      val c = env.lookup(TypeNameLookup(x.name.toQualifiedName)).get match {
                        case cd: ClassDeclaration => cd.body
                        case id: InterfaceDeclaration => id.body
                        case _ => throw new SyntaxError("Invalid declaration.")
                      }
                      env = c.scope.get
                    }
                  case Some(x:ClassType) => {
                      val c = env.lookup(TypeNameLookup(x.name.toQualifiedName)).get match {
                        case cd: ClassDeclaration => cd.body
                        case id: InterfaceDeclaration => id.body
                        case _ => throw new SyntaxError("Invalid declaration.")
                      }
                      env = c.scope.get
                    }

                  case Some(x:InterfaceType) => {
                      val i = env.lookup(TypeNameLookup(x.name.toQualifiedName)).get match {
                        case cd: ClassDeclaration => cd.body
                        case id: InterfaceDeclaration => id.body
                        case _ => throw new SyntaxError("Invalid declaration.")
                      }
                      env = i.scope.get
                    }
                  case _ => Unit
                }
              }

              if (env == e.scope.get) {
                None
              } else {
                val lookup = ExpressionNameLookup(ExpressionName(e.value).toQualifiedName)
                env.lookup(lookup) match {
                  case Some(f:FieldDeclaration) =>
                    if (f.modifiers.contains(FinalKeyword())) {
                      resolveConstantValue(f.expression.get)
                      f.expression.get.constantValue
                    } else {
                      None
                    }
                  case _ => None
                }
              }
            }
            case _ => None
          }
        }
        case _ => None
      }
    }
  }

  def validateMethodReturns(method: MethodDeclaration) : Unit = {
    if (!method.body.isEmpty && method.memberType != VoidKeyword()) {
      val statements : Seq[BlockStatement] = method.body.get.statements
      if (statements.isEmpty) {
          throw new SyntaxError("Missing return for method.")
      } else {
        if (!guaranteesReturns(statements.last)) {
          throw new SyntaxError("Missing return for method.")
        }
      }
    }
  }

  def guaranteesReturns(statement: BlockStatement) : Boolean = {
    statement match {
        case r:ReturnStatement => true
        case w:WhileStatement =>
          if (w.clause.constantValue == Some(ConstantBool("true"))) {
            // Never look at internals of internal loop.
            // Assume that a return exists in here
            true
          } else {
            // Either clause is constant false or we sometimes may enter it
            // In either case a return isn't sufficient
            false
          }
        case f:ForStatement =>
          if (!f.check.isEmpty && f.check.get.constantValue == Some(ConstantBool("true"))) {
            // Never look at internals of internal loop.
            // Assume that a return exists in here
            true
          } else {
            // Either check is constant false or we sometimes may enter it
            // In either case a return isn't sufficient
            false
          }
        case i:IfStatement =>
          if (i.trueCase.isEmpty || i.elseCase.isEmpty) {
            // There is a branch or two of logic that doesn't end in a return
            false
          } else {
            guaranteesReturns(i.trueCase.get) && guaranteesReturns(i.elseCase.get)
          }
        case b:Block =>
          val statements : Seq[BlockStatement] = b.statements
          if (statements.isEmpty) {
            false
          } else {
            guaranteesReturns(statements.last)
          }
        case _ =>
          false
      }
  }

  def validateAssignment(variable: LocalVariableDeclaration) : Unit = {
    val result:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(variable.name, variable.expression)
    result match {
      case Some(true) => Unit
      case Some(false) => throw new SyntaxError("Variable used in its own assignment.")
      case None => Unit
    }
  }


  def guaranteesDefiniteAssignmentBeforeUse(name:ExpressionName, node:AbstractSyntaxNode): Option[Boolean] = {
    node match {
      case c : ConditionalExpression => {
        var result:Option[Boolean] = None
        resolveConstantValue(c.e1)
        if (c.e1.constantValue.isEmpty) {
          result = guaranteesDefiniteAssignmentBeforeUse(name, c.e1)
        } else {
          c match {
            case _:OrExpression =>
              if (c.e1.constantValue.get.value.toBoolean) {
                result = None
              } else {
                result = guaranteesDefiniteAssignmentBeforeUse(name, c.e2)
              }
            case _:AndExpression =>
              if (!c.e1.constantValue.get.value.toBoolean) {
                result = None
              } else {
                result = guaranteesDefiniteAssignmentBeforeUse(name, c.e2)
              }
            case _:BinOrExpression =>
              if (c.e1.constantValue.get.value.toBoolean) {
                result = None
              } else {
                result = guaranteesDefiniteAssignmentBeforeUse(name, c.e2)
              }
            case _:BinAndExpression =>
              if (!c.e1.constantValue.get.value.toBoolean) {
                result = None
              } else {
                result = guaranteesDefiniteAssignmentBeforeUse(name, c.e2)
              }
            case _:BinXorExpression =>
              result = guaranteesDefiniteAssignmentBeforeUse(name, c.e1)
              result match {
                case Some(_) => Unit
                case None => result = guaranteesDefiniteAssignmentBeforeUse(name, c.e2)
              }
          }
        }
        result
      }

      case w:WhileStatement =>
        var result:Option[Boolean] = None
        w.children.foreach { child =>
          val temp:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, child)
          temp match {
            case Some(true) => Unit // names defined in loop scopes are not definitely assigned elsewhere
            case Some(false) => result = temp
            case None => Unit
          }
        }
        result

      case f:ForStatement =>
        var result:Option[Boolean] = None
        f.children.foreach { child =>
          val temp:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, child)
          temp match {
            case Some(true) => Unit // names defined in loop scopes are not definitely assigned elsewhere
            case Some(false) => result = temp
            case None => Unit
          }
        }
        result

      case i:IfStatement =>
        var result:Option[Boolean] = None
        i.children.foreach { child =>
          val temp:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, child)
          temp match {
            case Some(true) => Unit // names defined in if scopes are not definitely assigned elsewhere
            case Some(false) => result = temp
            case None => Unit
          }
        }
        result

      case n:ExpressionName =>
        if (n == name) {
          Some(false)
        } else {
          None
        }
      case f:FieldAccess =>
        val p:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, f.primary)
        if (p != None) {
          p
        } else {
          if (f.name == name.value) {
            Some(false)
          } else {
            None
          }
        }
      case a:SimpleArrayAccess =>
        val e:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, a.expr)
        if (e == None) {
          if (a.name == name) {
              Some(false)
          } else {
            None
          }
        } else {
          Some(false)
        }
      case a:ComplexArrayAccess =>
        val p:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, a.primary)
        val e:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, a.expr)
        if (e == None) {
          if (p == None) {
            None
          } else {
            Some(false)
          }
        } else {
          Some(false)
        }


      case a:Assignment =>
        val l:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, a.leftHandSide)
        val r:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, a.rightHandSide)
        a.leftHandSide match {
          case _:ExpressionName =>
           if (l == Some(false)) {
              if (r == None) {
                Some(true)
              } else {
                Some(false)
              }
            } else {
              r
            }
          case f:FieldAccess =>
            val p:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, f.primary)
            if (p == None) {
              if (f.name == name.value) {
                if (r == None) {
                  Some(true)
                } else {
                  Some(false)
                }
              } else {
                r
              }
            } else {
              Some(false)
            }
          case ar:SimpleArrayAccess =>
            val e:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, ar.expr)
            if (e == None) {
              if (ar.name == name) {
                if (r == None) {
                  Some(false)
                } else {
                  Some(false)
                }
              } else {
                r
              }
            } else {
              Some(false)
            }
          case ar:ComplexArrayAccess =>
            val p:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, ar.primary)
            val e:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, ar.expr)
            if (e == None) {
              if (p == Some(false)) {
                if (r == None) {
                  Some(false)
                } else {
                  Some(false)
                }
              } else {
                r
              }
            } else {
              Some(false)
            }
          case _ => None
        }
      case _ =>
        var result:Option[Boolean] = None
        node.children.foreach { child =>
          val temp:Option[Boolean] = guaranteesDefiniteAssignmentBeforeUse(name, child)
          temp match {
            case Some(true) => result = Some(true)
            case Some(false) =>
              if (result != Some(true)) {
                result = temp
              }
            case None => Unit
          }
        }
        result
    }
  }

  def validateReachability(node: AbstractSyntaxNode, codeFollows:Boolean): Unit = {
    node match {
      case w : WhileStatement =>
        resolveConstantValue(w.clause)
        w.clause.constantValue match {
            case Some(ConstantBool("false")) =>
              throw new SyntaxError("Unreachable body of while-loop statement.")
            case Some(ConstantBool("true")) =>
              if (codeFollows) {
                throw new SyntaxError("Unreachable statements after while-loop statement.")
              }
            case _ => Unit
        }
      case f : ForStatement =>
        if (!f.check.isEmpty) {
          resolveConstantValue(f.check.get)
          f.check.get.constantValue match {
              case Some(ConstantBool("false")) =>
                throw new SyntaxError("Unreachable body of for-loop statement.")
              case Some(ConstantBool("true")) =>
                if (codeFollows) {
                  throw new SyntaxError("Unreachable statements after for-loop statement.")
                }
                case _ => Unit
          }
        }
      case i : IfStatement =>
        if (!i.trueCase.isEmpty && !i.elseCase.isEmpty) {
          if (guaranteesReturns(i.trueCase.get) && guaranteesReturns(i.elseCase.get)) {
            if(codeFollows) {
              throw new SyntaxError("Unreachable statements after if statement.")
            }
          }
        }

      case r : ReturnStatement =>
        if(codeFollows) {
          throw new SyntaxError("Unreachable statements after return statement.")
        }

      case _ => Unit
    }
  }

  def check(node: AbstractSyntaxNode, parent: Option[AbstractSyntaxNode]) : Unit = {
    node.children.foreach { child => check(child, Some(node)) }

    def takeAfter[A](elem: A, seq:Seq[A]) : Seq[A] = {
      seq.drop(seq.indexOf(elem) + 1)
    }

    var siblings : Seq[AbstractSyntaxNode] = Seq.empty[AbstractSyntaxNode]
    if (!parent.isEmpty && parent.get.isInstanceOf[Block]) {
      siblings = takeAfter(node, parent.get.children)
    }

    node match {
        case w : WhileStatement => validateReachability(w, !siblings.isEmpty)
        case f : ForStatement => validateReachability(f, !siblings.isEmpty)
        case i : IfStatement => validateReachability(i, !siblings.isEmpty)
        case r : ReturnStatement => validateReachability(r, !siblings.isEmpty)

        case m: MethodDeclaration => validateMethodReturns(m)

        case v: LocalVariableDeclaration => validateAssignment(v)

        case _ => resolveConstantValue(node)
    }
  }
}
