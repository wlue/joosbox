package joosbox.parser

import joosbox.lexer.TokenType

abstract class ParseNodeType {
  def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[String] = None): ParseNode
  def tokenType: Option[TokenType]
}

abstract class ParseNode(val children: List[ParseNode] = List.empty[ParseNode], val value: Option[String] = None) {
  def tokenType: Option[TokenType]

  override def toString: String = if (children.size > 0) {
    if (value != None) {
      "ParseNodes." + this.getClass.getSimpleName + "(List[ParseNode](" + children.map(_.toString).mkString(",") + "), \"" + value + "\")"
    } else {
      "ParseNodes." + this.getClass.getSimpleName + "(List[ParseNode](" + children.map(_.toString).mkString(",") + "))"
    }
  } else {
    if (value != None) {
      "ParseNodes." + this.getClass.getSimpleName + "(List[ParseNode](), \"" + value + "\")"
    } else {
      "ParseNodes." + this.getClass.getSimpleName + "()"
    }
  }

  override def equals(obj: Any) = obj match {
    case node: ParseNode => node.value.equals(value) && node.children.equals(children)
    case _ => false
  }
}