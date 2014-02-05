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
  }
}
