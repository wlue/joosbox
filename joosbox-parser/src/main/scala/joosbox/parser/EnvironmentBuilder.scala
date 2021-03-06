package joosbox.parser

import joosbox.lexer.{
  SyntaxError,
  InputString
}
import AbstractSyntaxNode._
import joosbox.parser.ParseNodes.VoidKeyword

object EnvironmentBuilder {
  def build(nodes: Seq[CompilationUnit]): RootEnvironment = {
    val parent = new RootEnvironment(nodes)
    val astMapping:Seq[Map[AbstractSyntaxNode, Environment]] = nodes.map(traverse(_, parent, parent))

    val mapping:Map[AbstractSyntaxNode, Environment] = astMapping.reduce(_ ++ _)

    //  Set the parent's packageScopeMap to refer to the new scopes we have.
    parent.packageScopeMap = mapping.foldLeft(Map.empty[PackageName, Seq[ScopeEnvironment]]) {
      case (map: Map[PackageName, Seq[ScopeEnvironment]], (asn: CompilationUnit, env: Environment)) => {
        env match {
          case scope: ScopeEnvironment => {
            asn.packageDeclaration match {
              case Some(PackageDeclaration(pn: PackageName)) => {
                val prefixMap: Map[PackageName, Seq[ScopeEnvironment]] =
                  pn.toQualifiedName.prefixes.map { prefix => 
                    (prefix.toPackageName, map.getOrElse(prefix.toPackageName, Seq.empty[ScopeEnvironment]))
                  }.toMap
                map ++ prefixMap + (pn -> (map.getOrElse(pn, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }

              case None => {
                val implicitPackage = PackageName(InputString(""))
                map + (implicitPackage -> (map.getOrElse(implicitPackage, Seq.empty[ScopeEnvironment]) ++ Seq(scope)))
              }
            }
          }
          case _ => map
        }
      }
      case (map: Map[PackageName, Seq[ScopeEnvironment]], (asn: AbstractSyntaxNode, env: Environment)) => map
    }

    mapping.values.toSet[Environment].collect({ case s: ScopeEnvironment => s }).foreach(s => {
      s.ensureUnambiguousReferences(mapping)
      s.linkScopesWithMapping(mapping)
      s.importScopeReferences.foreach((q: PackageNameLookup) => {
        parent.packageScopeMap.get(q.name) match {
          case None => throw new SyntaxError("Attempted on-demand import " + q.name + " could not be found.")
          case Some(_) => Unit
        }
      })
    })

    parent.resolveFullyQualifiedNames()

    parent
  }

  def traverse(node: AbstractSyntaxNode, parent: Environment, root: RootEnvironment): Map[AbstractSyntaxNode, Environment] = {
    val r = node match {
      //  Special case for Blocks - we need to unroll their contents and make nested scopes.
      case Block(seq: Seq[BlockStatement]) => {

        val e = new ScopeEnvironment(Map.empty, None, Seq.empty, parent)
        val scopetree = scopeTreeFromBlockStatements(seq, e, root)
        if (node.scope == None) {
          node.scope = Some(e)
        }

        node.children.flatMap(traverse(_, e, root)).toMap ++ scopetree ++ Map(node -> e)
      }

      case cd: ClassDeclaration => {
        val linkedScopeReferences: Seq[EnvironmentLookup] = {
          (cd.superclass.toSeq ++ cd.interfaces).flatMap {
            case ClassType(tn: TypeName) => Some(TypeNameLookup(tn.toQualifiedName))
            case InterfaceType(tn: TypeName) => Some(TypeNameLookup(tn.toQualifiedName))
            case _ => None
          } ++ Seq(TypeNameLookup(CommonNames.JavaLangObject))
        }

        val declarationScope = new ScopeEnvironment(Map.empty, None, Seq.empty, parent, Some(cd), linkedScopeReferences)
        val n: ClassBody = cd.body

        val methodMapping: Map[EnvironmentLookup, Referenceable]
          = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
            case (map: Map[EnvironmentLookup, Referenceable], cd: ConstructorDeclaration) => {
              if (cd.parameters.map(_.name).toSet.size != cd.parameters.size) {
                throw new SyntaxError("Duplicate parameter names in constructor.")
              }

              val key = ConstructorLookup(cd.name.toQualifiedName, cd.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate constructor declaration for " + cd.name)
                case None => map + (key -> cd)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], md: MethodDeclaration) => {
              if (md.parameters.map(_.name).toSet.size != md.parameters.size) {
                throw new SyntaxError("Duplicate parameter names in constructor.")
              }

              val key = MethodLookup(md.name.toQualifiedName, md.parameters.map(_.varType))
              map.get(key) match {
                case Some(_) => throw new SyntaxError("Duplicate method declaration for " + md.name)
                case None => map + (key -> md)
              }
            }
            case (map: Map[EnvironmentLookup, Referenceable], asn: AbstractSyntaxNode) => map
          })

        val staticFields:Seq[FieldDeclaration] = n.declarations.collect({
          case f: FieldDeclaration if f.modifiers.collectFirst{case StaticKeyword() => true}.isDefined => f
        })
        val instanceFields:Seq[FieldDeclaration] = n.declarations.collect({
          case f: FieldDeclaration if !f.modifiers.collectFirst{case StaticKeyword() => true}.isDefined => f
        })

        val fieldEnvironments = scopeTreeFromFieldDeclarations(
          staticFields ++ instanceFields, declarationScope, root, Some(cd)
        )

        val rightMostEnvironment = fieldEnvironments.lastOption match {
          case Some((_ :AbstractSyntaxNode, e: Environment)) => e
          case None => declarationScope
        }

        //  Methods live in an environment "to the right"
        //  of all of the fields - that is, after they have
        //  all been defined.
        val methodEnvironment = new ScopeEnvironment(methodMapping, None, Seq.empty, rightMostEnvironment, Some(cd), linkedScopeReferences)
        cd.scope = Some(methodEnvironment)
        n.scope = Some(methodEnvironment)
        cd.name.scope = Some(parent)

        // Assignment to other fields within field initializers should ignore ordering of fields.
        (staticFields ++ instanceFields).foreach(f => {
          f.expression match {
            case Some(ex: Expression) => {
              def flattenTree(n: AbstractSyntaxNode): Seq[AbstractSyntaxNode] = Seq(n) ++ n.children.flatMap(flattenTree)
              flattenTree(ex).foreach {
                case Assignment(lhs: ExpressionName, rhs: AbstractSyntaxNode) => {
                  lhs.scope = Some(rightMostEnvironment)
                }
                case _ => Unit
              }
            }
            case None => Unit
          }
        })

        (
          Map(cd -> methodEnvironment, n -> methodEnvironment)
          ++ fieldEnvironments
          ++ methodMapping.values.flatMap(traverse(_, methodEnvironment, root))
          ++ (cd.superclass.toSeq ++ cd.modifiers.toSeq ++ cd.interfaces.toSeq).flatMap(traverse(_, methodEnvironment, root))
        )
      }

      case cd: InterfaceDeclaration => {
        val linkedScopeReferences: Seq[EnvironmentLookup] = {
          cd.interfaces.flatMap {
            case InterfaceType(tn: TypeName) => Some(TypeNameLookup(tn.toQualifiedName))
            case _ => None
          } ++ Seq(TypeNameLookup(CommonNames.JavaLangObject))
        }

        val n: InterfaceBody = cd.body

        val methodMapping: Map[EnvironmentLookup, Referenceable]
        = n.declarations.foldLeft(Map.empty[EnvironmentLookup, Referenceable])({
          case (map: Map[EnvironmentLookup, Referenceable], md: InterfaceMemberDeclaration) => {
            if (md.parameters.map(_.name).toSet.size != md.parameters.size) {
              throw new SyntaxError("Duplicate parameter names in constructor.")
            }

            val key = MethodLookup(md.name.toQualifiedName, md.parameters.map(_.varType))
            map.get(key) match {
              case Some(_) => throw new SyntaxError("Duplicate interface member declaration for " + md.name)
              case None => map + (key -> md)
            }
          }
        })

        val methodEnvironment = new ScopeEnvironment(methodMapping, None, Seq.empty, parent, Some(cd), linkedScopeReferences)
        cd.scope = Some(methodEnvironment)
        n.scope = Some(methodEnvironment)
        cd.name.scope = Some(parent)

        (
          Map(cd -> methodEnvironment, n -> methodEnvironment)
            ++ methodMapping.values.flatMap(traverse(_, methodEnvironment, root))
            ++ (cd.modifiers.toSeq ++ cd.interfaces.toSeq).flatMap(traverse(_, methodEnvironment, root))
          )
      }

      case node: AbstractSyntaxNode => {
        val e = environmentFromNode(node, parent)
        if (node.scope == None) {
          node.scope = Some(e)
        }
        Map(node -> e) ++ node.children.flatMap(traverse(_, e, root))
      }
    }
    if (node.scope == None) {
      throw new SyntaxError("Node scope is None - this should never happen!")
    }
    r
  }

  def scopeTreeFromFieldDeclarations(
    statements: Seq[FieldDeclaration],
    parent: Environment,
    root: RootEnvironment,
    classDeclaration: Option[ClassDeclaration]
  ): Seq[(AbstractSyntaxNode, Environment)] = {
    statements.headOption match {
      case None => Seq.empty[(AbstractSyntaxNode, Environment)]
      case Some(fd: FieldDeclaration) => {
        val fieldLookup = ExpressionNameLookup(fd.name.toQualifiedName)
        if (parent.lookup(fieldLookup).isDefined) {
          throw new SyntaxError("Duplicate definition of field: " + fd.name)
        }

        val fieldEnvironment =
          new ScopeEnvironment(Map(fieldLookup -> fd), None, Seq.empty, parent, classDeclaration)

        if (fd.scope == None) {
          fd.scope = Some(fieldEnvironment)
        }

        val rhsMap: Map[EnvironmentLookup, Referenceable]
          = if (fd.modifiers.collectFirst{case StaticKeyword() => true}.isEmpty && classDeclaration.isDefined) {
          Map(ExpressionNameLookup(QualifiedName(Seq(InputString("this")))) -> classDeclaration.get)
        } else {
          Map.empty[EnvironmentLookup, Referenceable]
        }
        val rhsEnvironment =
          new ScopeEnvironment(rhsMap, None, Seq.empty, parent, classDeclaration)

        //  Children of this declaration should have their RHS environments set to
        //  something distinct, but need not be returned.
        fd.children.foreach(traverse(_, rhsEnvironment, root))

        (
          Seq((fd, fieldEnvironment))
          ++ scopeTreeFromFieldDeclarations(statements.drop(1), fieldEnvironment, root, classDeclaration)
        )
      }
    }
  }

  def scopeTreeFromBlockStatements(
    statements: Seq[BlockStatement],
    parent: Environment,
    root: RootEnvironment
  ): Map[AbstractSyntaxNode, Environment] = {
    statements match {
      case Seq() => Map.empty[AbstractSyntaxNode, Environment]
      case _ => {
        val (preDecls, decls) = statements.span { 
          case x: LocalVariableDeclaration => false
          case ForStatement(Some(v: ForVariableDeclaration), _, _, _) => false
          case _ => true
        }

        decls.headOption match {
          case Some(decl: LocalVariableDeclaration) => {
            //  Declaration implicitly creates a new scope for itself and everything after it.
            val key = ExpressionNameLookup(decl.name.toQualifiedName)
            parent.lookup(key) match {
              case Some(local: LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + local + ")")
              case Some(forv: ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.name + " (previous definition: " + forv + ")")
              case Some(param: FormalParameter)
                => throw new SyntaxError("Redefinition of " + decl.name + " (conflicts with parameter: " + param + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), None, Seq.empty, parent)
                if (decl.scope == None) {
                  decl.scope = Some(env)
                }
                (
                  preDecls.flatMap(traverse(_, parent, root)).toMap
                  ++ Map(decl -> env) ++ decl.children.flatMap(traverse(_, env, root)).toMap
                  ++ scopeTreeFromBlockStatements(decls.drop(1), env, root)
                )
              }
            }
          }

          case Some(ForStatement(
            Some(decl: ForVariableDeclaration),
            check: Option[Expression],
            update: Option[StatementExpression],
            statement: Statement
          )) => {
            val key = ExpressionNameLookup(decl.variableName.toQualifiedName)
            parent.lookup(key) match {
              case Some(local: LocalVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + local + ")")
              case Some(forv: ForVariableDeclaration)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (previous definition: " + forv + ")")
              case Some(param: FormalParameter)
                => throw new SyntaxError("Redefinition of " + decl.variableName + " (conflicts with parameter: " + param + ")")
              case _ => {
                val env = new ScopeEnvironment(Map(key -> decl), None, Seq.empty, parent)
                if (decl.scope == None) {
                  decl.scope = Some(env)
                }
                val r = (
                  Map(decl -> env)
                  ++ decl.children.flatMap(traverse(_, env, root)).toMap
                  ++ (check.toSeq ++ update.toSeq ++ Seq(statement)).flatMap(traverse(_, env, root)).toMap
                  ++ scopeTreeFromBlockStatements(decls.drop(1), parent, root)
                )
                r
              }
            }
          }

          //  If there is no assignment, group all of the following
          //  statements into the same scope and continue traversing.
          case _ => statements.flatMap(s => traverse(s, parent, root)).toMap
        }
      }
    }
  }

  def environmentFromNode(node: AbstractSyntaxNode, parent: Environment): Environment = {
    node match {
      case n: CompilationUnit => {
        //  Locals of a CompilationUnit should contain all of its
        //  defined classes and interfaces, as well as all of its
        //  explicit imports.

        //  otherScopes of a CompilationUnit should include:
        //      All other scopes in its package
        //    followed by
        //      All * imports.

        //  All single type imports should be fully qualified.
        val explicitImports: Map[EnvironmentLookup, Referenceable] = n.importDeclarations.flatMap {
          case i: SingleTypeImportDeclaration => {
            i.name match {
              case t: TypeName => {
                parent.search(TypeNameLookup(t.toQualifiedName)) match {
                  case Some(r: Referenceable) => Some(TypeNameLookup(TypeName(t.value).toQualifiedName) -> r)
                  case None => throw new SyntaxError("Type import '" + i.name + "' not found in " + parent)
                }
              }
            }
          }
          case _ => None
        }.toMap

        val locals: Map[EnvironmentLookup, Referenceable] =
          explicitImports ++ Map(TypeNameLookup(n.typeDeclaration.name.toQualifiedName) -> n.typeDeclaration)

//        val locals: Map[EnvironmentLookup, Referenceable] =
//          explicitImports ++ (TypeNameLookup(n.typeDeclaration.name.toQualifiedName) -> n.typeDeclaration).toMap

        val packageScopeReference: PackageNameLookup = {
          n.packageDeclaration match {
            case Some(PackageDeclaration(pn: PackageName)) => PackageNameLookup(pn)

            //  The "default package", the empty string.
            case None => PackageNameLookup(PackageName(InputString("")))
          }
        }

        val importScopeReferences: Seq[PackageNameLookup] = {
          val imports:Seq[PackageNameLookup] = n.importDeclarations.flatMap {
            case TypeImportOnDemandDeclaration(pn: PackageName) => Some(PackageNameLookup(pn))
            case SingleTypeImportDeclaration(tn: TypeName) => None
          }

          Seq(PackageNameLookup(QualifiedName(Seq(InputString("java"), InputString("lang"))).toPackageName)) ++ imports
        }

        n.scope = Some(new ScopeEnvironment(locals, Some(packageScopeReference), importScopeReferences, parent, Some(n)))
        n.scope.get
      }

      case n: MethodDeclaration => {
        val parameterMapping: Map[EnvironmentLookup, Referenceable]
          = n.parameters.map(fp => (ExpressionNameLookup(fp.name.toQualifiedName), fp)).toMap
        val mapping = if (n.modifiers.collectFirst{case StaticKeyword() => true}.isDefined) {
          parameterMapping
        } else {
          parent.getEnclosingClassNode match {
            case Some(classDeclaration: ClassDeclaration) => {
              val thisExpression = QualifiedName(Seq(InputString("this"))).toExpressionName
              parameterMapping + (ExpressionNameLookup(thisExpression.toQualifiedName) -> classDeclaration)
            }
            case _ => parameterMapping
          }
        }
        n.scope = Some(new ScopeEnvironment(mapping, None, Seq.empty, parent))
        n.scope.get
      }

      case c: ConstructorDeclaration => {
        val parameterMapping: Map[EnvironmentLookup, Referenceable]
        = c.parameters.map(fp => (ExpressionNameLookup(fp.name.toQualifiedName), fp)).toMap
        val mapping = parent.getEnclosingClassNode match {
          case Some(classDeclaration: ClassDeclaration) => {
            val thisExpression = QualifiedName(Seq(InputString("this"))).toExpressionName
            parameterMapping + (ExpressionNameLookup(thisExpression.toQualifiedName) -> classDeclaration)
          }
          case _ => parameterMapping
        }
        c.scope = Some(new ScopeEnvironment(mapping, None, Seq.empty, parent))
        c.scope.get
      }

      case f: FieldDeclaration => {
        f.scope = Some(if (f.modifiers.collectFirst{case StaticKeyword() => true}.isDefined) {
          parent
        } else {
          parent.getEnclosingClassNode match {
            case Some(classDeclaration: ClassDeclaration) => {
              val thisExpression = QualifiedName(Seq(InputString("this"))).toExpressionName
              val mapping: Map[EnvironmentLookup, Referenceable] = Map(ExpressionNameLookup(thisExpression.toQualifiedName) -> classDeclaration)
              new ScopeEnvironment(mapping, None, Seq.empty, parent)
            }
            case _ => parent
          }
        })
        f.scope.get
      }

      case n: AbstractSyntaxNode => parent
    }
  }
}
