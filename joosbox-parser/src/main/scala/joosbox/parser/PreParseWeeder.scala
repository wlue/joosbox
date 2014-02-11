package joosbox.parser

import joosbox.lexer.TokenTypes
import joosbox.lexer.TokenType
import joosbox.lexer.Token
import joosbox.lexer.SyntaxError

object PreParseWeeder {
  def verify(input: Token): Option[Token] = input.tokenType match {
    case TokenTypes.AbstractKeyword   |
         TokenTypes.Assign            |
         TokenTypes.BOF               |
         TokenTypes.BinaryAnd         |
         TokenTypes.BinaryOr          |
         TokenTypes.BinaryXor         |
         TokenTypes.BooleanKeyword    |
         TokenTypes.ByteKeyword       |
         TokenTypes.CharKeyword       |
         TokenTypes.CharLiteral       |
         TokenTypes.ClassKeyword      |
         TokenTypes.Comma             |
         TokenTypes.Divide            |
         TokenTypes.Dot               |
         TokenTypes.EOF               |
         TokenTypes.ElseKeyword       |
         TokenTypes.Equal             |
         TokenTypes.ExtendsKeyword    |
         TokenTypes.FalseLiteral      |
         TokenTypes.FinalKeyword      |
         TokenTypes.ForKeyword        |
         TokenTypes.GreaterEqual      |
         TokenTypes.GreaterThan       |
         TokenTypes.Identifier        |
         TokenTypes.IfKeyword         |
         TokenTypes.ImplementsKeyword |
         TokenTypes.ImportKeyword     |
         TokenTypes.InstanceofKeyword |
         TokenTypes.IntKeyword        |
         TokenTypes.InterfaceKeyword  |
         TokenTypes.LeftBracket       |
         TokenTypes.LeftCurly         |
         TokenTypes.LeftParen         |
         TokenTypes.LessEqual         |
         TokenTypes.LessThan          |
         TokenTypes.LogicalAnd        |
         TokenTypes.LogicalNot        |
         TokenTypes.LogicalOr         |
         TokenTypes.Minus             |
         TokenTypes.Modulo            |
         TokenTypes.NativeKeyword     |
         TokenTypes.NewKeyword        |
         TokenTypes.NotEqual          |
         TokenTypes.NullLiteral       |
         TokenTypes.Num               |
         TokenTypes.PackageKeyword    |
         TokenTypes.Plus              |
         TokenTypes.ProtectedKeyword  |
         TokenTypes.PublicKeyword     |
         TokenTypes.ReturnKeyword     |
         TokenTypes.RightBracket      |
         TokenTypes.RightCurly        |
         TokenTypes.RightParen        |
         TokenTypes.S                 |
         TokenTypes.Semicolon         |
         TokenTypes.ShortKeyword      |
         TokenTypes.Star              |
         TokenTypes.StaticKeyword     |
         TokenTypes.StringLiteral     |
         TokenTypes.ThisKeyword       |
         TokenTypes.TrueLiteral       |
         TokenTypes.VoidKeyword       |
         TokenTypes.WhileKeyword        => Some(input)

    //  These tokens are ignored by the parser.
    case TokenTypes.Whitespace          |
         TokenTypes.SingleLineComment   |
         TokenTypes.MultiLineComment    => None

    case _ => throw SyntaxError(input, "Not valid in the Joos grammar.")
  }
}
