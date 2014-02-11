package joosbox.parser

import joosbox.lexer.TokenType
import joosbox.lexer.InputString

trait ParseNodeType {
  def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[InputString] = None): ParseNode
  def name: String = this.getClass.getSimpleName.replace("$", "")
  def tokenType: Option[TokenType]
}
