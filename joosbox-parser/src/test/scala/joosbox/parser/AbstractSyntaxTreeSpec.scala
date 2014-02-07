package joosbox.parser.test

import org.specs2.mutable._
import joosbox.parser._
import joosbox.lexer._
import InputStringImplicits._

class AbstractSyntaxTreeSpec extends Specification {
  
  "Abstract Syntax Tree" should {
    
    "be created from a parse tree with package and import" in {
      val parseTree: ParseNode = ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](

          ParseNodes.PackageDeclaration(List[ParseNode](
            ParseNodes.PackageKeyword(List.empty[ParseNode], Some("package")),
            ParseNodes.Name(List.empty[ParseNode], Some("mypackage")),
            ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
          )),

          ParseNodes.ImportDeclarations(List[ParseNode](
            ParseNodes.ImportDeclaration(List[ParseNode](
              ParseNodes.SingleTypeImportDeclaration(List[ParseNode](
                ParseNodes.ImportKeyword(List.empty[ParseNode], Some("import")),
                ParseNodes.Name(List.empty[ParseNode], Some("myimport")),
                ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
              ))
            ))
          )),

          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
                ParseNodes.Modifiers(List[ParseNode](
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                    ))
                  ))
                )),
                ParseNodes.ClassKeyword(List.empty[ParseNode], Some("class")),
                ParseNodes.Identifier(List.empty[ParseNode], Some("identifier")),
                ParseNodes.ClassBody(List[ParseNode](
                  ParseNodes.LeftCurly(List.empty[ParseNode], Some("{")),
                  ParseNodes.RightCurly(List.empty[ParseNode], Some("}"))
                ))
              ))
            ))
          ))

        )),
        ParseNodes.EOF()
      ))

      AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
        AbstractSyntaxNode.CompilationUnit(
          Some(AbstractSyntaxNode.PackageDeclaration("mypackage")),
          List(AbstractSyntaxNode.SingleTypeImportDeclaration("myimport")),
          List(AbstractSyntaxNode.ClassDeclaration(
            "identifier",
            AbstractSyntaxNode.ClassBody(),
            Set(AbstractSyntaxNode.PublicKeyword)
          ))
        )
      )
    }

    "be created from a parse tree with package and multiple imports" in {
      val parseTree:ParseNode = ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](

          ParseNodes.PackageDeclaration(List[ParseNode](
            ParseNodes.PackageKeyword(List.empty[ParseNode], Some("package")),
            ParseNodes.Name(List.empty[ParseNode], Some("mypackage")),
            ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
          )),

          ParseNodes.ImportDeclarations(List[ParseNode](
            ParseNodes.ImportDeclarations(List[ParseNode](
              ParseNodes.ImportDeclaration(List[ParseNode](
                ParseNodes.SingleTypeImportDeclaration(List[ParseNode](
                  ParseNodes.ImportKeyword(List.empty[ParseNode], Some("import")),
                  ParseNodes.Name(List.empty[ParseNode], Some("myotherimport")),
                  ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
                ))
              ))
            )),
            ParseNodes.ImportDeclaration(List[ParseNode](
              ParseNodes.SingleTypeImportDeclaration(List[ParseNode](
                ParseNodes.ImportKeyword(List.empty[ParseNode], Some("import")),
                ParseNodes.Name(List.empty[ParseNode], Some("myimport")),
                ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
              ))
            ))
          )),

          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
                ParseNodes.Modifiers(List[ParseNode](
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                    ))
                  ))
                )),
                ParseNodes.ClassKeyword(List.empty[ParseNode], Some("class")),
                ParseNodes.Identifier(List.empty[ParseNode], Some("classname")),
                ParseNodes.ClassBody(List[ParseNode](
                  ParseNodes.LeftCurly(List.empty[ParseNode], Some("{")),
                  ParseNodes.RightCurly(List.empty[ParseNode], Some("}"))
                ))
              ))
            ))
          ))

        )),
        ParseNodes.EOF()
      ))

      AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
        AbstractSyntaxNode.CompilationUnit(
          Some(AbstractSyntaxNode.PackageDeclaration("mypackage")),
          List(
            AbstractSyntaxNode.SingleTypeImportDeclaration("myotherimport"),
            AbstractSyntaxNode.SingleTypeImportDeclaration("myimport")
          ),
          List(AbstractSyntaxNode.ClassDeclaration(
            "classname",
            AbstractSyntaxNode.ClassBody(),
            Set(AbstractSyntaxNode.PublicKeyword)
          ))
        )
      )
    }

    "be created from a parse tree with modifiers on a class" in {
      val parseTree:ParseNode = ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](

          ParseNodes.PackageDeclaration(List[ParseNode](
            ParseNodes.PackageKeyword(List.empty[ParseNode], Some("package")),
            ParseNodes.Name(List.empty[ParseNode], Some("mypackage")),
            ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
          )),

          ParseNodes.ImportDeclarations(List[ParseNode](
            ParseNodes.ImportDeclaration(List[ParseNode](
              ParseNodes.SingleTypeImportDeclaration(List[ParseNode](
                ParseNodes.ImportKeyword(List.empty[ParseNode], Some("import")),
                ParseNodes.Name(List.empty[ParseNode], Some("myimport")),
                ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
              ))
            ))
          )),

          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
                ParseNodes.Modifiers(List[ParseNode](
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.StaticKeyword(List.empty[ParseNode], Some("static"))
                    ))
                  )),
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                    ))
                  ))
                )),
                ParseNodes.ClassKeyword(List.empty[ParseNode], Some("class")),
                ParseNodes.Identifier(List.empty[ParseNode], Some("identifier")),
                ParseNodes.ClassBody(List[ParseNode](
                  ParseNodes.LeftCurly(List.empty[ParseNode], Some("{")),
                  ParseNodes.RightCurly(List.empty[ParseNode], Some("}"))
                ))
              ))
            ))
          ))

        )),
        ParseNodes.EOF()
      ))

      AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
        AbstractSyntaxNode.CompilationUnit(
          Some(AbstractSyntaxNode.PackageDeclaration("mypackage")),
          List(AbstractSyntaxNode.SingleTypeImportDeclaration("myimport")),
          List(AbstractSyntaxNode.ClassDeclaration(
            "identifier",
            AbstractSyntaxNode.ClassBody(),
            Set(AbstractSyntaxNode.StaticKeyword, AbstractSyntaxNode.PublicKeyword)
          ))
        )
      )
    }

    "fail to be created from a parse tree with invalid modifiers on a class" in {
      val parseTree:ParseNode = ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](

          ParseNodes.PackageDeclaration(List[ParseNode](
            ParseNodes.PackageKeyword(List.empty[ParseNode], Some("package")),
            ParseNodes.Name(List.empty[ParseNode], Some("mypackage")),
            ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
          )),

          ParseNodes.ImportDeclarations(List[ParseNode](
            ParseNodes.ImportDeclaration(List[ParseNode](
              ParseNodes.SingleTypeImportDeclaration(List[ParseNode](
                ParseNodes.ImportKeyword(List.empty[ParseNode], Some("import")),
                ParseNodes.Name(List.empty[ParseNode], Some("myimport")),
                ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
              ))
            ))
          )),

          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
                ParseNodes.Modifiers(List[ParseNode](
                  ParseNodes.Modifiers(List[ParseNode](
                    ParseNodes.Modifier(List[ParseNode](
                      ParseNodes.AccessModifier(List[ParseNode](
                        ParseNodes.AbstractKeyword(List.empty[ParseNode], Some("abstract"))
                      ))
                    ))
                  )),
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.FinalKeyword(List.empty[ParseNode], Some("final"))
                    ))
                  )),
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                    ))
                  ))
                )),
                ParseNodes.ClassKeyword(List.empty[ParseNode], Some("class")),
                ParseNodes.Identifier(List.empty[ParseNode], Some("identifier")),
                ParseNodes.ClassBody(List[ParseNode](
                  ParseNodes.LeftCurly(List.empty[ParseNode], Some("{")),
                  ParseNodes.RightCurly(List.empty[ParseNode], Some("}"))
                ))
              ))
            ))
          ))

        )),
        ParseNodes.EOF()
      ))

      AbstractSyntaxNode.parse(parseTree).head must throwA[SyntaxError]
    }

    "be created from a class with fields and methods" in {
      val parseTree:ParseNode = ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](

          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
                ParseNodes.Modifiers(List[ParseNode](
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.StaticKeyword(List.empty[ParseNode], Some("static"))
                    ))
                  )),
                  ParseNodes.Modifier(List[ParseNode](
                    ParseNodes.AccessModifier(List[ParseNode](
                      ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                    ))
                  ))
                )),
                ParseNodes.ClassKeyword(List.empty[ParseNode], Some("class")),
                ParseNodes.Identifier(List.empty[ParseNode], Some("identifier")),
                ParseNodes.ClassBody(List[ParseNode](
                  ParseNodes.LeftCurly(List.empty[ParseNode], Some("{")),
                  ParseNodes.ClassBodyDeclarations(List[ParseNode](
                    ParseNodes.ClassBodyDeclaration(List[ParseNode](
                      ParseNodes.ClassMemberDeclaration(List[ParseNode](
                        ParseNodes.FieldDeclaration(List[ParseNode](

                          ParseNodes.Modifiers(List[ParseNode](
                            ParseNodes.Modifier(List[ParseNode](
                              ParseNodes.AccessModifier(List[ParseNode](
                                ParseNodes.PublicKeyword(List.empty[ParseNode], Some("public"))
                              ))
                            ))
                          )),

                          ParseNodes.Type(List[ParseNode](
                            ParseNodes.ReferenceType(List[ParseNode](
                              ParseNodes.ClassOrInterfaceType(List[ParseNode](
                                ParseNodes.Name(List[ParseNode](
                                  ParseNodes.SimpleName(List[ParseNode](
                                    ParseNodes.Identifier(List.empty[ParseNode], Some("String"))
                                  ))
                                ))
                              ))
                            ))
                          )),

                          ParseNodes.VariableDeclarators(List[ParseNode](
                            ParseNodes.VariableDeclarator(List[ParseNode](
                              ParseNodes.VariableDeclaratorId(List[ParseNode](
                                ParseNodes.Identifier(List.empty[ParseNode], Some("myVariable"))
                              ))
                              /*,ParseNodes.Assign(List.empty[ParseNode], Some("=")),
                              ParseNodes.Expression(List[ParseNode](

                              ))*/
                            ))
                          )),

                          ParseNodes.Semicolon(List.empty[ParseNode], Some(";"))
                        ))
                      ))
                    ))
                  )),
                  ParseNodes.RightCurly(List.empty[ParseNode], Some("}"))
                ))
              ))
            ))
          ))

        )),
        ParseNodes.EOF()
      ))

      AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
        AbstractSyntaxNode.CompilationUnit(
          None,
          List.empty[AbstractSyntaxNode.ImportDeclaration],
          List(AbstractSyntaxNode.ClassDeclaration(
            "identifier",
            AbstractSyntaxNode.ClassBody(
              Seq(
                AbstractSyntaxNode.FieldDeclaration(
                  "myVariable",
                  Set(AbstractSyntaxNode.PublicKeyword),
                  AbstractSyntaxNode.SimpleName("String")
                )
              )
            ),
            Set(AbstractSyntaxNode.StaticKeyword, AbstractSyntaxNode.PublicKeyword)
          ))
        )
      )
    }

    "literals" in {
      "Num" in {
        "valid" in {
          val parseTree: ParseNode =
            ParseNodes.Literal(List(
              ParseNodes.IntegerLiteral(List(
                ParseNodes.Num(Nil, Some("1337"))
              ))
            ))

          AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
            AbstractSyntaxNode.Num("1337", "1337")
          )
        }

        "underflow" in {
          "don't throw exception for positive overflow with a negative" in {
            val parseTree: ParseNode =
              ParseNodes.UnaryExpression(List(
                ParseNodes.Minus(Nil),
                ParseNodes.UnaryExpression(List(
                  ParseNodes.UnaryExpressionNotPlusMinus(List(
                    ParseNodes.PostfixExpression(List(
                      ParseNodes.Primary(List(
                        ParseNodes.PrimaryNoNewArray(List(
                          ParseNodes.Literal(List(
                            ParseNodes.IntegerLiteral(List(
                              ParseNodes.Num(Nil, Some("2147483648"))
                            ))
                          ))
                        ))
                      ))
                    ))
                  ))
                ))
              ))

            AbstractSyntaxNode.parse(parseTree).head must not(throwA[Exception])

            val result = AbstractSyntaxNode.parse(parseTree).head
            result must beEqualTo(AbstractSyntaxNode.Num("-2147483648", "2147483648"))
          }

          "throw exception for negative underflow" in {
            val parseTree: ParseNode =
              ParseNodes.UnaryExpression(List(
                ParseNodes.Minus(Nil),
                ParseNodes.UnaryExpression(List(
                  ParseNodes.UnaryExpressionNotPlusMinus(List(
                    ParseNodes.PostfixExpression(List(
                      ParseNodes.Primary(List(
                        ParseNodes.PrimaryNoNewArray(List(
                          ParseNodes.Literal(List(
                            ParseNodes.IntegerLiteral(List(
                              ParseNodes.Num(Nil, Some("2147483649"))
                            ))
                          ))
                        ))
                      ))
                    ))
                  ))
                ))
              ))

            AbstractSyntaxNode.parse(parseTree).head must throwA[Exception]
          }
        }

        "overflow" in {
          val parseTree: ParseNode =
            ParseNodes.Literal(List(
              ParseNodes.IntegerLiteral(List(
                ParseNodes.Num(Nil, Some("2147483648"))
              ))
            ))

          AbstractSyntaxNode.parse(parseTree).head must throwA[Exception]
        }

        "-10" in {
          val parseTree: ParseNode =
            ParseNodes.UnaryExpression(List(
              ParseNodes.Minus(Nil),
              ParseNodes.UnaryExpression(List(
                ParseNodes.UnaryExpressionNotPlusMinus(List(
                  ParseNodes.PostfixExpression(List(
                    ParseNodes.Primary(List(
                      ParseNodes.PrimaryNoNewArray(List(
                        ParseNodes.Literal(List(
                          ParseNodes.IntegerLiteral(List(
                            ParseNodes.Num(Nil, Some("10"))
                          ))
                        ))
                      ))
                    ))
                  ))
                ))
              ))
            ))

          AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
            AbstractSyntaxNode.Num("-10", "10")
          )
        }

        "- -10" in {
          val parseTree: ParseNode =
            ParseNodes.UnaryExpression(List(
              ParseNodes.Minus(Nil),
              ParseNodes.UnaryExpression(List(
                ParseNodes.Minus(Nil),
                ParseNodes.UnaryExpression(List(
                  ParseNodes.UnaryExpressionNotPlusMinus(List(
                    ParseNodes.PostfixExpression(List(
                      ParseNodes.Primary(List(
                        ParseNodes.PrimaryNoNewArray(List(
                          ParseNodes.Literal(List(
                            ParseNodes.IntegerLiteral(List(
                              ParseNodes.Num(Nil, Some("10"))
                            ))
                          ))
                        ))
                      ))
                    ))
                  ))
                ))
              ))
            ))

          AbstractSyntaxNode.parse(parseTree).head must beEqualTo(
            AbstractSyntaxNode.Num("10", "10")
          )
        }
      }
    }
  }
}
