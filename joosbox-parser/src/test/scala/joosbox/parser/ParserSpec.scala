package joosbox.parser.test

import org.specs2.mutable._
import joosbox.parser._
import joosbox.lexer._

class ParserSpec extends Specification {
  "Parser" should {
    
    "read in full LR1 grammar" in {
      Parser.fromLR1File("joos1w.lr1") must not(throwA[Exception])
    }

    "parse full LR1 grammar" in {
      val p: Parser = Parser.Joos

      val result: ParseNode = p.parse(List(
        Tokens.ClassKeyword("class"),
        Tokens.Identifier("identifier"),
        Tokens.LeftCurly("{"),
        Tokens.RightCurly("}")
      ))
      result must beEqualTo(ParseNodes.S(List[ParseNode](
        ParseNodes.BOF(),
        ParseNodes.CompilationUnit(List[ParseNode](
          ParseNodes.TypeDeclarations(List[ParseNode](
            ParseNodes.TypeDeclaration(List[ParseNode](
              ParseNodes.ClassDeclaration(List[ParseNode](
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
      )))
    }

    "parse full LR1 grammar and then syntax error" in {
      val p: Parser = Parser.Joos

      p.parse(List(
        Tokens.PackageKeyword("package"),
        Tokens.Identifier("mypackage"),
        Tokens.Semicolon(";"),
        Tokens.ImportKeyword("import"), 
        Tokens.ClassKeyword("class"),
        Tokens.Identifier("myclass"),
        Tokens.LeftCurly("{"),
        Tokens.RightCurly("}")
      )) must throwA[SyntaxError]
    }

    "scan and parse a valid Joos program on the LR1 grammar" in {
      val test : String = """
class Test {
    public static void main(String args[]) {
        System.out.println("Hello world!");
    }
}
      """
      Parser.Joos.parseString(test) must not(throwA[SyntaxError])
    }

    "scan and parse a valid Joos program on the LR1 grammar" in {
      val test : String = """
class Test {
    public static void main(String[] args) {
        System.out.println("Hello world!");
    }
}
      """
      Parser.Joos.parseString(test) must not(throwA[SyntaxError])
    }

    "scan and parse a valid Joos program on the LR1 grammar" in {
      val test : String = """
class Test {
    public static void main(String args) {
        System.out.println("Hello world!");
        System.out.println("Hello world!");
        System.out.println("Hello world!");
        System.out.println("Hello world!");
    }
}
      """

      Parser.Joos.parseString(test) must not(throwA[SyntaxError])
    }

    "flatten a parse tree" in {
      val test : String = """
class Test {
    public static void main(String args[]) {
        System.out.println("Hello world!");
    }
}
      """
      Parser.Joos.parseString(test).flatten must beEqualTo(Some(
        ParseNodes.S(List[ParseNode](
          ParseNodes.ClassDeclaration(List[ParseNode](
            ParseNodes.ClassKeyword(List[ParseNode](), Some("class")),
            ParseNodes.Identifier(List[ParseNode](), Some("Test")),
            ParseNodes.ClassBody(List[ParseNode](
              ParseNodes.LeftCurly(List[ParseNode](), Some("{")),
              ParseNodes.MethodDeclaration(List[ParseNode](
                ParseNodes.MethodHeader(List[ParseNode](
                  ParseNodes.Modifiers(List[ParseNode](
                    ParseNodes.PublicKeyword(List[ParseNode](), Some("public")),
                    ParseNodes.StaticKeyword(List[ParseNode](), Some("static"))
                  )),
                  ParseNodes.VoidKeyword(List[ParseNode](), Some("void")),
                  ParseNodes.MethodDeclarator(List[ParseNode](
                    ParseNodes.Identifier(List[ParseNode](), Some("main")),
                    ParseNodes.LeftParen(List[ParseNode](), Some("(")),
                      ParseNodes.FormalParameter(List[ParseNode](
                        ParseNodes.Identifier(List[ParseNode](), Some("String")),
                        ParseNodes.VariableDeclaratorId(List[ParseNode](
                          ParseNodes.Identifier(List[ParseNode](), Some("args")),
                          ParseNodes.LeftBracket(List[ParseNode](), Some("[")),
                          ParseNodes.RightBracket(List[ParseNode](), Some("]"))
                        ))
                      )),
                      ParseNodes.RightParen(List[ParseNode](), Some(")"))
                    ))
                )),
                ParseNodes.Block(List[ParseNode](
                  ParseNodes.LeftCurly(List[ParseNode](), Some("{")),
                  ParseNodes.ExpressionStatement(List[ParseNode](
                    ParseNodes.MethodInvocation(List[ParseNode](
                      ParseNodes.QualifiedName(List[ParseNode](
                        ParseNodes.QualifiedName(List[ParseNode](
                          ParseNodes.Identifier(List[ParseNode](), Some("System")),
                          ParseNodes.Dot(List[ParseNode](), Some(".")),
                          ParseNodes.Identifier(List[ParseNode](), Some("out"))
                        )),
                        ParseNodes.Dot(List[ParseNode](), Some(".")),
                        ParseNodes.Identifier(List[ParseNode](), Some("println"))
                      )),
                      ParseNodes.LeftParen(List[ParseNode](), Some("(")),
                      ParseNodes.StringLiteral(List[ParseNode](), Some("\"Hello world!\"")),
                      ParseNodes.RightParen(List[ParseNode](), Some(")"))
                    )),
                    ParseNodes.Semicolon(List[ParseNode](), Some(";"))
                  )),
                  ParseNodes.RightCurly(List[ParseNode](), Some("}"))
                ))
              )),
              ParseNodes.RightCurly(List[ParseNode](), Some("}"))
            ))
          ))
        ))
      ))
    }
    "marmoset tests for assignment 1" in {
      val parser: Parser = Parser.Joos
      "J1_01" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_01.java") must not(throwA[SyntaxError])
      }
      "J1_1_AmbiguousName_AccessResultFromMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_AmbiguousName_AccessResultFromMethod.java") must not(throwA[SyntaxError])
      }
      "J1_1_Cast_Complement" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Cast_Complement.java") must not(throwA[SyntaxError])
      }
      "J1_1_Cast_MultipleCastOfSameValue_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Cast_MultipleCastOfSameValue_1.java") must not(throwA[SyntaxError])
      }
      "J1_1_Cast_MultipleCastOfSameValue_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Cast_MultipleCastOfSameValue_2.java") must not(throwA[SyntaxError])
      }
      "J1_1_Cast_MultipleCastOfSameValue_3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Cast_MultipleCastOfSameValue_3.java") must not(throwA[SyntaxError])
      }
      "J1_1_Cast_MultipleReferenceArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Cast_MultipleReferenceArray.java") must not(throwA[SyntaxError])
      }
      "J1_1_Escapes_3DigitOctalAndDigit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Escapes_3DigitOctalAndDigit.java") must not(throwA[SyntaxError])
      }
      "J1_1_Instanceof_InLazyExp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Instanceof_InLazyExp.java") must not(throwA[SyntaxError])
      }
      "J1_1_Instanceof_OfAdditiveExpression" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Instanceof_OfAdditiveExpression.java") must not(throwA[SyntaxError])
      }
      "J1_1_Instanceof_OfCastExpression" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_Instanceof_OfCastExpression.java") must not(throwA[SyntaxError])
      }
      "J1_1_IntRange_NegativeInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_1_IntRange_NegativeInt.java") must not(throwA[SyntaxError])
      }
      "J1_abstractclass" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_abstractclass.java") must not(throwA[SyntaxError])
      }
      "J1_abstractmethodwithoutbody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_abstractmethodwithoutbody.java") must not(throwA[SyntaxError])
      }
      "J1_arbitrarylocaldeclaration" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_arbitrarylocaldeclaration.java") must not(throwA[SyntaxError])
      }
      "J1_arithmeticoperations" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_arithmeticoperations.java") must not(throwA[SyntaxError])
      }
      "J1_ArrayCreateAndIndex" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_ArrayCreateAndIndex.java") must not(throwA[SyntaxError])
      }
      "J1_assignment" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_assignment.java") must not(throwA[SyntaxError])
      }
      "J1_assignmentExp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_assignmentExp.java") must not(throwA[SyntaxError])
      }
      "J1_barminusfoo" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_barminusfoo.java") must not(throwA[SyntaxError])
      }
      "J1_BigInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_BigInt.java") must not(throwA[SyntaxError])
      }
      "J1_char" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_char.java") must not(throwA[SyntaxError])
      }
      "J1_char_escape" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_char_escape.java") must not(throwA[SyntaxError])
      }
      "J1_char_escape2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_char_escape2.java") must not(throwA[SyntaxError])
      }
      "J1_char_escape3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_char_escape3.java") must not(throwA[SyntaxError])
      }
      "J1_charadd" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_charadd.java") must not(throwA[SyntaxError])
      }
      "J1_CharCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_CharCast.java") must not(throwA[SyntaxError])
      }
      "J1_CharCharInit1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_CharCharInit1.java") must not(throwA[SyntaxError])
      }
      "J1_CharCharInit2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_CharCharInit2.java") must not(throwA[SyntaxError])
      }
      "J1_charliterals" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_charliterals.java") must not(throwA[SyntaxError])
      }
      "J1_classinstance" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_classinstance.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp1.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp2.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp3.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp4" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp4.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp5" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp5.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp6" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp6.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp7" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp7.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp8" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp8.java") must not(throwA[SyntaxError])
      }
      "J1_commentsInExp9" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_commentsInExp9.java") must not(throwA[SyntaxError])
      }
      "J1_comparisonoperations" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_comparisonoperations.java") must not(throwA[SyntaxError])
      }
      "J1_concat_in_binop" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_concat_in_binop.java") must not(throwA[SyntaxError])
      }
      "J1_constructorbodycast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_constructorbodycast.java") must not(throwA[SyntaxError])
      }
      "J1_constructorparameter" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_constructorparameter.java") must not(throwA[SyntaxError])
      }
      "J1_constructorWithSameNameAsMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_constructorWithSameNameAsMethod.java") must not(throwA[SyntaxError])
      }
      "J1_eagerbooleanoperations" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_eagerbooleanoperations.java") must not(throwA[SyntaxError])
      }
      "J1_EscapeEscape" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_EscapeEscape.java") must not(throwA[SyntaxError])
      }
      "J1_evalMethodInvocationFromParExp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_evalMethodInvocationFromParExp.java") must not(throwA[SyntaxError])
      }
      "J1_exp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_exp.java") must not(throwA[SyntaxError])
      }
      "J1_extends" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_extends.java") must not(throwA[SyntaxError])
      }
      "J1_externalcall" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_externalcall.java") must not(throwA[SyntaxError])
      }
      "J1_finalclass" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_finalclass.java") must not(throwA[SyntaxError])
      }
      "J1_finalclass2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_finalclass2.java") must not(throwA[SyntaxError])
      }
      "J1_for_no_short_if" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_for_no_short_if.java") must not(throwA[SyntaxError])
      }
      "J1_forAllwaysInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forAllwaysInit.java") must not(throwA[SyntaxError])
      }
      "J1_forAlwaysInitAsWhile" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forAlwaysInitAsWhile.java") must not(throwA[SyntaxError])
      }
      "J1_forbodycast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forbodycast.java") must not(throwA[SyntaxError])
      }
      "J1_forifstatements1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forifstatements1.java") must not(throwA[SyntaxError])
      }
      "J1_forifstatements2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forifstatements2.java") must not(throwA[SyntaxError])
      }
      "J1_forifstatements3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forifstatements3.java") must not(throwA[SyntaxError])
      }
      "J1_forinfor" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forinfor.java") must not(throwA[SyntaxError])
      }
      "J1_forinitcast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forinitcast.java") must not(throwA[SyntaxError])
      }
      "J1_forMethodInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forMethodInit.java") must not(throwA[SyntaxError])
      }
      "J1_forMethodUpdate" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forMethodUpdate.java") must not(throwA[SyntaxError])
      }
      "J1_forMethodUpdate2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forMethodUpdate2.java") must not(throwA[SyntaxError])
      }
      "J1_ForUpdate_ClassCreation" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_ForUpdate_ClassCreation.java") must not(throwA[SyntaxError])
      }
      "J1_forupdatecast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forupdatecast.java") must not(throwA[SyntaxError])
      }
      "J1_forWithoutExp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forWithoutExp.java") must not(throwA[SyntaxError])
      }
      "J1_forWithoutInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forWithoutInit.java") must not(throwA[SyntaxError])
      }
      "J1_forWithoutUpdate" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_forWithoutUpdate.java") must not(throwA[SyntaxError])
      }
      "J1_hello_comment" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_hello_comment.java") must not(throwA[SyntaxError])
      }
      "J1_if" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_if.java") must not(throwA[SyntaxError])
      }
      "J1_if_then" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_if_then.java") must not(throwA[SyntaxError])
      }
      "J1_if_then_for" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_if_then_for.java") must not(throwA[SyntaxError])
      }
      "J1_ifThenElse" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_ifThenElse.java") must not(throwA[SyntaxError])
      }
      "J1_implements" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_implements.java") must not(throwA[SyntaxError])
      }
      "J1_IntArrayDecl1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntArrayDecl1.java") must not(throwA[SyntaxError])
      }
      "J1_IntArrayDecl2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntArrayDecl2.java") must not(throwA[SyntaxError])
      }
      "J1_IntCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntCast.java") must not(throwA[SyntaxError])
      }
      "J1_IntCharInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntCharInit.java") must not(throwA[SyntaxError])
      }
      "J1_integerFun" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_integerFun.java") must not(throwA[SyntaxError])
      }
      "J1_integerFun1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_integerFun1.java") must not(throwA[SyntaxError])
      }
      "J1_integerFun3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_integerFun3.java") must not(throwA[SyntaxError])
      }
      "J1_IntInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntInit.java") must not(throwA[SyntaxError])
      }
      "J1_intliterals" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_intliterals.java") must not(throwA[SyntaxError])
      }
      "J1_intminusfoo" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_intminusfoo.java") must not(throwA[SyntaxError])
      }
      "J1_IntRange_MinNegativeInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IntRange_MinNegativeInt.java") must not(throwA[SyntaxError])
      }
      "J1_IsThisACast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_IsThisACast.java") must not(throwA[SyntaxError])
      }
      "J1_lazybooleanoperations" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_lazybooleanoperations.java") must not(throwA[SyntaxError])
      }
      "J1_maxint_comment" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_maxint_comment.java") must not(throwA[SyntaxError])
      }
      "J1_minuschar" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_minuschar.java") must not(throwA[SyntaxError])
      }
      "J1_minusminusminus" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_minusminusminus.java") must not(throwA[SyntaxError])
      }
      "J1_NamedTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NamedTypeArray.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeByteCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeByteCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeCharCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeCharCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeIntCast1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeIntCast1.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeIntCast2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeIntCast2.java") must not(throwA[SyntaxError])
      }
      "J1_negativeintcast3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_negativeintcast3.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeOneByteByteCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeOneByteByteCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeOneByteCharCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeOneByteCharCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeOneByteIntCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeOneByteIntCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeOneByteShortCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeOneByteShortCast.java") must not(throwA[SyntaxError])
      }
      "J1_NegativeShortCast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_NegativeShortCast.java") must not(throwA[SyntaxError])
      }
      "J1_newobject" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_newobject.java") must not(throwA[SyntaxError])
      }
      "J1_nonemptyconstructor" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_nonemptyconstructor.java") must not(throwA[SyntaxError])
      }
      "J1_nullinstanceof1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_nullinstanceof1.java") must not(throwA[SyntaxError])
      }
      "J1_nullliteral" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_nullliteral.java") must not(throwA[SyntaxError])
      }
      "J1_octal_escape" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_octal_escape.java") must not(throwA[SyntaxError])
      }
      "J1_octal_escape2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_octal_escape2.java") must not(throwA[SyntaxError])
      }
      "J1_octal_escape3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_octal_escape3.java") must not(throwA[SyntaxError])
      }
      "J1_octal_escape4" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_octal_escape4.java") must not(throwA[SyntaxError])
      }
      "J1_octal_escape5" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_octal_escape5.java") must not(throwA[SyntaxError])
      }
      "J1_primitivecasts" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_primitivecasts.java") must not(throwA[SyntaxError])
      }
      "J1_protected" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_protected.java") must not(throwA[SyntaxError])
      }
      "J1_protectedfields" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_protectedfields.java") must not(throwA[SyntaxError])
      }
      "J1_publicclasses" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_publicclasses.java") must not(throwA[SyntaxError])
      }
      "J1_publicconstructors" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_publicconstructors.java") must not(throwA[SyntaxError])
      }
      "J1_publicfields" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_publicfields.java") must not(throwA[SyntaxError])
      }
      "J1_publicmethods" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_publicmethods.java") must not(throwA[SyntaxError])
      }
      "J1_SimpleTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_SimpleTypeArray.java") must not(throwA[SyntaxError])
      }
      "J1_SmallInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_SmallInt.java") must not(throwA[SyntaxError])
      }
      "J1_staticmethoddeclaration" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_staticmethoddeclaration.java") must not(throwA[SyntaxError])
      }
      "J1_stringliteralinvoke" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_stringliteralinvoke.java") must not(throwA[SyntaxError])
      }
      "J1_stringliterals" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_stringliterals.java") must not(throwA[SyntaxError])
      }
      "J1_weird_chars" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1_weird_chars.java") must not(throwA[SyntaxError])
      }
      "J1w_Interface" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1w_Interface.java") must not(throwA[SyntaxError])
      }
      "J1w_RestrictedNative" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1w_RestrictedNative.java") must not(throwA[SyntaxError])
      }
      "J1w_StaticField" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J1w_StaticField.java") must not(throwA[SyntaxError])
      }
      "J2_staticFieldDecl" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J2_staticFieldDecl.java") must throwA[SyntaxError]
      }
      "J2_staticfielddeclaration" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/J2_staticfielddeclaration.java") must throwA[SyntaxError]
      }
      "Je_16_Circularity_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Circularity_1.java") must throwA[SyntaxError]
      }
      "Je_16_Circularity_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Circularity_2.java") must throwA[SyntaxError]
      }
      "Je_16_Circularity_3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Circularity_3.java") must throwA[SyntaxError]
      }
      "Je_16_Circularity_4_Rhoshaped" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Circularity_4_Rhoshaped.java") must throwA[SyntaxError]
      }
      "Je_16_ClosestMatch_Array" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_ClosestMatch_Array.java") must throwA[SyntaxError]
      }
      "Je_16_ClosestMatch_Constructor_NoClosestMatch_This" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_ClosestMatch_Constructor_NoClosestMatch_This.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_ArrayLengthDec" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_ArrayLengthDec.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_ArrayLengthInc" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_ArrayLengthInc.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_PostDec" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_PostDec.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_PostInc" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_PostInc.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_PreDec" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_PreDec.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_Final_PreInc" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_Final_PreInc.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_StringPostDec" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_StringPostDec.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_StringPostInc" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_StringPostInc.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_StringPreDec" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_StringPreDec.java") must throwA[SyntaxError]
      }
      "Je_16_IncDec_StringPreInc" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_IncDec_StringPreInc.java") must throwA[SyntaxError]
      }
      "Je_16_MultiArrayCreation_Assign_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_MultiArrayCreation_Assign_1.java") must throwA[SyntaxError]
      }
      "Je_16_MultiArrayCreation_Null" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_MultiArrayCreation_Null.java") must throwA[SyntaxError]
      }
      "Je_16_StaticThis_ArgumentToSuper" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_StaticThis_ArgumentToSuper.java") must throwA[SyntaxError]
      }
      "Je_16_StaticThis_ArgumentToThis" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_StaticThis_ArgumentToThis.java") must throwA[SyntaxError]
      }
      "Je_16_SuperThis_InvalidSuperParameter" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_SuperThis_InvalidSuperParameter.java") must throwA[SyntaxError]
      }
      "Je_16_SuperThis_InvalidThisParameter" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_SuperThis_InvalidThisParameter.java") must throwA[SyntaxError]
      }
      "Je_16_Throw_NoThrows" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Throw_NoThrows.java") must throwA[SyntaxError]
      }
      "Je_16_Throw_NotSubclass" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Throw_NotSubclass.java") must throwA[SyntaxError]
      }
      "Je_16_Throw_SimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Throw_SimpleType.java") must throwA[SyntaxError]
      }
      "Je_16_Throw_ThrowsNotSuperclass" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Throw_ThrowsNotSuperclass.java") must throwA[SyntaxError]
      }
      "Je_16_Throws_This" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_16_Throws_This.java") must throwA[SyntaxError]
      }
      "Je_17_Unreachable_AfterThrow" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_17_Unreachable_AfterThrow.java") must throwA[SyntaxError]
      }
      "Je_17_Unreachable_AfterThrowInConditional" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_17_Unreachable_AfterThrowInConditional.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractClass_AbstractConstructor" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractClass_AbstractConstructor.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractClass_Final" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractClass_Final.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractMethod_Body" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractMethod_Body.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractMethod_EmptyBody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractMethod_EmptyBody.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractMethod_Final" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractMethod_Final.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractMethod_Static" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractMethod_Static.java") must throwA[SyntaxError]
      }
      "Je_1_AbstractMethodCannotBeFinal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_AbstractMethodCannotBeFinal.java") must throwA[SyntaxError]
      }
      "Je_1_Access_PrivateLocal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Access_PrivateLocal.java") must throwA[SyntaxError]
      }
      "Je_1_Access_ProtectedLocal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Access_ProtectedLocal.java") must throwA[SyntaxError]
      }
      "Je_1_Access_PublicLocal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Access_PublicLocal.java") must throwA[SyntaxError]
      }
      "Je_1_Array_Data" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Array_Data.java") must throwA[SyntaxError]
      }
      "Je_1_Array_Data_Empty" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Array_Data_Empty.java") must throwA[SyntaxError]
      }
      "Je_1_Array_OnVariableNameInDecl" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Array_OnVariableNameInDecl.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_DoubleParenthese" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_DoubleParenthese.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_Expression" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_Expression.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_LeftHandSideOfAssignment_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_LeftHandSideOfAssignment_1.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_LeftHandSideOfAssignment_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_LeftHandSideOfAssignment_2.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_NonstaticField" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_NonstaticField.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_NoParenthesis" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_NoParenthesis.java") must throwA[SyntaxError]
      }
      "Je_1_Cast_ToMethodInvoke" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Cast_ToMethodInvoke.java") must throwA[SyntaxError]
      }
      "Je_1_CastToArrayLvalue" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_CastToArrayLvalue.java") must throwA[SyntaxError]
      }
      "Je_1_ClassDeclaration_WrongFileName" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_ClassDeclaration_WrongFileName.java") must not(throwA[SyntaxError])
      }
      "Je_1_ClassDeclaration_WrongFileName_Dot.foo" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_ClassDeclaration_WrongFileName_Dot.foo.java") must not(throwA[SyntaxError])
      }
      "Je_1_ClassDeclaration_WrongFileName_Suffix" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_ClassDeclaration_WrongFileName_Suffix.java") must not(throwA[SyntaxError])
      }
      "Je_1_ClassInstantiation_InstantiateSimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_ClassInstantiation_InstantiateSimpleType.java") must throwA[SyntaxError]
      }
      "Je_1_ClassInstantiation_InstantiateSimpleValue" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_ClassInstantiation_InstantiateSimpleValue.java") must throwA[SyntaxError]
      }
      "Je_1_Declarations_MultipleVars" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Declarations_MultipleVars.java") must throwA[SyntaxError]
      }
      "Je_1_Declarations_MultipleVars_Fields" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Declarations_MultipleVars_Fields.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_1DigitOctal_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_1DigitOctal_1.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_1DigitOctal_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_1DigitOctal_2.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_1DigitOctal_3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_1DigitOctal_3.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_1DigitOctal_4" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_1DigitOctal_4.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_2DigitOctal_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_2DigitOctal_1.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_2DigitOctal_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_2DigitOctal_2.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_2DigitOctal_3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_2DigitOctal_3.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_3DigitOctal_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_3DigitOctal_1.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_3DigitOctal_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_3DigitOctal_2.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_3DigitOctal_3" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_3DigitOctal_3.java") must throwA[SyntaxError]
      }
      "Je_1_Escapes_NonExistingEscape" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Escapes_NonExistingEscape.java") must throwA[SyntaxError]
      }
      "Je_1_Extends_NamedTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Extends_NamedTypeArray.java") must throwA[SyntaxError]
      }
      "Je_1_Extends_SimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Extends_SimpleType.java") must throwA[SyntaxError]
      }
      "Je_1_Extends_SimpleTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Extends_SimpleTypeArray.java") must throwA[SyntaxError]
      }
      "Je_1_Extends_Value" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Extends_Value.java") must throwA[SyntaxError]
      }
      "Je_1_FinalField_NoInitializer" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_FinalField_NoInitializer.java") must throwA[SyntaxError]
      }
      "Je_1_For_DeclarationInUpdate" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_DeclarationInUpdate.java") must throwA[SyntaxError]
      }
      "Je_1_For_MultipleDeclarationsInInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_MultipleDeclarationsInInit.java") must throwA[SyntaxError]
      }
      "Je_1_For_MultipleUpdates" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_MultipleUpdates.java") must throwA[SyntaxError]
      }
      "Je_1_For_NotAStatementInUpdate" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_NotAStatementInUpdate.java") must throwA[SyntaxError]
      }
      "Je_1_For_PrimaryExpInInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_PrimaryExpInInit.java") must throwA[SyntaxError]
      }
      "Je_1_For_PrimaryExpInUpdate" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_PrimaryExpInUpdate.java") must throwA[SyntaxError]
      }
      "Je_1_For_StatementInInit" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_For_StatementInInit.java") must throwA[SyntaxError]
      }
      "Je_1_Formals_Final" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Formals_Final.java") must throwA[SyntaxError]
      }
      "Je_1_Formals_Initializer_Constructor" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Formals_Initializer_Constructor.java") must throwA[SyntaxError]
      }
      "Je_1_Formals_Initializer_Method" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Formals_Initializer_Method.java") must throwA[SyntaxError]
      }
      "Je_1_Identifiers_Goto" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Identifiers_Goto.java") must throwA[SyntaxError]
      }
      "Je_1_Identifiers_Private" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Identifiers_Private.java") must throwA[SyntaxError]
      }
      "Je_1_Implements_NamedTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Implements_NamedTypeArray.java") must throwA[SyntaxError]
      }
      "Je_1_Implements_SimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Implements_SimpleType.java") must throwA[SyntaxError]
      }
      "Je_1_Implements_SimpleTypeArray" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Implements_SimpleTypeArray.java") must throwA[SyntaxError]
      }
      "Je_1_Implements_Value" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Implements_Value.java") must throwA[SyntaxError]
      }
      "Je_1_IncDec_IncDecNotLvalue" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IncDec_IncDecNotLvalue.java") must throwA[SyntaxError]
      }
      "Je_1_IncDec_Parenthesized" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IncDec_Parenthesized.java") must throwA[SyntaxError]
      }
      "Je_1_InstanceInitializers" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_InstanceInitializers.java") must throwA[SyntaxError]
      }
      "Je_1_InstanceOf_Null" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_InstanceOf_Null.java") must throwA[SyntaxError]
      }
      "Je_1_InstanceOf_Primitive" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_InstanceOf_Primitive.java") must throwA[SyntaxError]
      }
      "Je_1_InstanceOf_Void" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_InstanceOf_Void.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_ConstructorAbstract" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_ConstructorAbstract.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_ConstructorBody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_ConstructorBody.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_Field" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_Field.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_FinalMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_FinalMethod.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_MethodBody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_MethodBody.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_NoBody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_NoBody.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_StaticMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_StaticMethod.java") must throwA[SyntaxError]
      }
      "Je_1_Interface_WrongFileName" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Interface_WrongFileName.java") must throwA[SyntaxError]
      }
      "Je_1_IntRange_MinusTooBigInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IntRange_MinusTooBigInt.java") must not(throwA[SyntaxError])
      }
      "Je_1_IntRange_PlusTooBigInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IntRange_PlusTooBigInt.java") must not(throwA[SyntaxError])
      }
      "Je_1_IntRange_TooBigInt" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IntRange_TooBigInt.java") must not(throwA[SyntaxError])
      }
      "Je_1_IntRange_TooBigInt_InInitializer" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IntRange_TooBigInt_InInitializer.java") must not(throwA[SyntaxError])
      }
      "Je_1_IntRange_TooBigIntNegated" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_IntRange_TooBigIntNegated.java") must not(throwA[SyntaxError])
      }
      "Je_1_JoosTypes_Double" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_JoosTypes_Double.java") must throwA[SyntaxError]
      }
      "Je_1_JoosTypes_Float" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_JoosTypes_Float.java") must throwA[SyntaxError]
      }
      "Je_1_JoosTypes_Long" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_JoosTypes_Long.java") must throwA[SyntaxError]
      }
      "Je_1_LabeledStatements" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_LabeledStatements.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Class" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Class.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Exponential" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Exponential.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Float" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Float.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Hex" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Hex.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Long" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Long.java") must throwA[SyntaxError]
      }
      "Je_1_Literals_Octal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Literals_Octal.java") must throwA[SyntaxError]
      }
      "Je_1_Locals_Final" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Locals_Final.java") must throwA[SyntaxError]
      }
      "Je_1_Methods_MissingAccessModifier" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Methods_MissingAccessModifier.java") must throwA[SyntaxError]
      }
      "Je_1_Methods_NonAbstractNoBody" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Methods_NonAbstractNoBody.java") must throwA[SyntaxError]
      }
      "Je_1_Methods_StaticFinal" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Methods_StaticFinal.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayCreation_Assign_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayCreation_Assign_2.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayCreation_MissingDimension_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayCreation_MissingDimension_1.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayCreation_MissingDimension_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayCreation_MissingDimension_2.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayCreation_MissingDimension_4" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayCreation_MissingDimension_4.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayCreation_NoType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayCreation_NoType.java") must throwA[SyntaxError]
      }
      "Je_1_MultiArrayTypes_Dimensions" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_MultiArrayTypes_Dimensions.java") must throwA[SyntaxError]
      }
      "Je_1_NegIntTooLow" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NegIntTooLow.java") must not(throwA[SyntaxError])
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_BitwiseAnd" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_BitwiseAnd.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_BitwiseOr" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_BitwiseOr.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_BitwiseXOR" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_BitwiseXOR.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_Divide" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_Divide.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_Minus" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_Minus.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_Multiply" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_Multiply.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_Plus" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_Plus.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_Remainder" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_Remainder.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_ShiftLeft" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_ShiftLeft.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_SignShiftRight" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_SignShiftRight.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_AssignmentOperations_ZeroShiftRight" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_AssignmentOperations_ZeroShiftRight.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_BitShift_Left" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_BitShift_Left.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_BitShift_SignRight" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_BitShift_SignRight.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_BitShift_ZeroRight" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_BitShift_ZeroRight.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Bitwise_Negation" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Bitwise_Negation.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Break" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Break.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Choice" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Choice.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Continue" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Continue.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_DoWhile" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_DoWhile.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_ExpressionSequence" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_ExpressionSequence.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_MultipleTypesPrFile" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_MultipleTypesPrFile.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_NestedTypes" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_NestedTypes.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_PrivateFields" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_PrivateFields.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_PrivateMethods" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_PrivateMethods.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_StaticInitializers" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_StaticInitializers.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Strictftp" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Strictftp.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_SuperMethodCall" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_SuperMethodCall.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Switch" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Switch.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Synchronized" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Synchronized.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_SynchronizedStatement" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_SynchronizedStatement.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Transient" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Transient.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_UnaryPlus" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_UnaryPlus.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Unicode" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Unicode.java") must throwA[SyntaxError]
      }
      "Je_1_NonJoosConstructs_Volatile" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_NonJoosConstructs_Volatile.java") must throwA[SyntaxError]
      }
      "Je_1_PackagePrivate_Class" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_PackagePrivate_Class.java") must throwA[SyntaxError]
      }
      "Je_1_PackagePrivate_Field" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_PackagePrivate_Field.java") must throwA[SyntaxError]
      }
      "Je_1_PackagePrivate_Method" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_PackagePrivate_Method.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_SuperAfterBlock" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_SuperAfterBlock.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_SuperAfterStatement" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_SuperAfterStatement.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_SuperInBlock" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_SuperInBlock.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_SuperInMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_SuperInMethod.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_SuperThis" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_SuperThis.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_ThisAfterStatement" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_ThisAfterStatement.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_ThisInMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_ThisInMethod.java") must throwA[SyntaxError]
      }
      "Je_1_SuperThis_TwoSuperCalls" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_SuperThis_TwoSuperCalls.java") must throwA[SyntaxError]
      }
      "Je_1_Throw_NotExpression" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Throw_NotExpression.java") must throwA[SyntaxError]
      }
      "Je_1_Throws_Array" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Throws_Array.java") must throwA[SyntaxError]
      }
      "Je_1_Throws_SimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Throws_SimpleType.java") must throwA[SyntaxError]
      }
      "Je_1_Throws_Void" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_Throws_Void.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_ArrayCreation" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_ArrayCreation.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_ArrayDeclaration" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_ArrayDeclaration.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_Cast" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_Cast.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_Field" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_Field.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_Formals" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_Formals.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_Local" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_Local.java") must throwA[SyntaxError]
      }
      "Je_1_VoidType_VoidMethod" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_1_VoidType_VoidMethod.java") must throwA[SyntaxError]
      }
      "Je_6_Assignable_Instanceof_SimpleTypeOfSimpleType" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_6_Assignable_Instanceof_SimpleTypeOfSimpleType.java") must throwA[SyntaxError]
      }
      "Je_6_InstanceOf_Primitive_1" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_6_InstanceOf_Primitive_1.java") must throwA[SyntaxError]
      }
      "Je_6_InstanceOf_Primitive_2" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_6_InstanceOf_Primitive_2.java") must throwA[SyntaxError]
      }
      "Je_Native" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_Native.java") must not(throwA[SyntaxError])
      }
      "Je_Throws" in {
        parser.parseFilename("joosbox-compiler/src/test/resources/marmoset-tests/a1/Je_Throws.java") must not(throwA[SyntaxError])
      }
    }
  }
}
