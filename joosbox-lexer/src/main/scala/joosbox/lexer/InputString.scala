package joosbox.lexer

case class InputString(
  value: String,
  filename: String = "<input>",
  line: Int = 1,
  idx: Int = 0
) {
  override def toString: String = "\"" + value + "\" (" + filename + ":" + line + " char " + idx + ")"
  override def equals(o: Any) = o match {
    case i: InputString => i.value.equalsIgnoreCase(this.value)
    case _ => false
  }
}

object InputStringImplicits {
  import scala.language.implicitConversions
  implicit def stringToInputString(string: String): InputString = InputString(string)
  implicit def pairToInputString(pair: Pair[String, Int]): InputString = InputString(pair._1, "<input>", 1, pair._2)
  implicit def tupleToInputString(tuple: (String, Int, Int)): InputString = InputString(tuple._1, "<input>", tuple._2, tuple._3)
}
