package joosbox.parser

import joosbox.lexer.TokenType
import joosbox.lexer.InputString

trait ParseNodeType {
  def apply(children: List[ParseNode] = List.empty[ParseNode], value: Option[InputString] = None): ParseNode
  def name: String = this.getClass.getSimpleName.replace("$", "")
  def tokenType: Option[TokenType]
}

abstract class ParseNode(
  val children: List[ParseNode] = List.empty[ParseNode],
  val value: Option[InputString] = None
) {
  def tokenType: Option[TokenType]

  override def toString: String = {
    if (children.size > 0) {
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
  }

  override def equals(obj: Any) = obj match {
    case node: ParseNode => node.value.equals(value) && node.children.equals(children)
    case _ => false
  }

  def instantiateThis(args: AnyRef*): ParseNode = instantiate(getClass)(args: _*)

  def instantiate(clazz: java.lang.Class[_ <: ParseNode])(args: AnyRef*): ParseNode = {
    val constructor = clazz.getConstructors()(0)
    constructor.newInstance(args: _*).asInstanceOf[ParseNode]
  }

  def flatten: Option[ParseNode] = {
    if (value != None || children.size > 1) {
      Some(instantiateThis(children.flatMap(_.flatten).toList, value))
    } else {
      if (children.size == 1) {
        children.head.flatten
      } else {
        None
      }
    }
  }
}
