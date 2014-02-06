package joosbox.parser.test

import org.specs2.mutable._
import joosbox.parser._
import joosbox.lexer._
import InputStringImplicits._

class AbstractSyntaxTreeSpec extends Specification {
  "Abstract Syntax Tree" should {
    
    "be created from a simple parse tree" in {
      val parseTree:ParseNode = ParseNodes.S(List[ParseNode](
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
      ))

      println(AbstractSyntaxNode.fromParseNode(parseTree))
      true

    }

    "be created from a parse tree with package and import" in {
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

      println(AbstractSyntaxNode.fromParseNode(parseTree))
      true

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

      println(AbstractSyntaxNode.fromParseNode(parseTree))
      true

    }
  }
}
