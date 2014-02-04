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
      val p: Parser = Parser.fromLR1File("joos1w.lr1")

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
      val p: Parser = Parser.fromLR1File("joos1w.lr1")

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
  }
}
