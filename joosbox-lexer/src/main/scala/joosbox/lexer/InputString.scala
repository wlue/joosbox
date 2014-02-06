package joosbox.lexer

case class InputString(value: String, val filename: String = "<input>", val line: Int = 1, val idx: Int = 0) {
  override def toString: String = "\"" + value + "\" (" + filename + ":" + line + " char " + idx + ")"
}

object InputStringImplicits {
  import scala.language.implicitConversions
  implicit def stringToInputString(string: String): InputString = InputString(string)
  implicit def pairToInputString(pair: Pair[String, Int]): InputString = InputString(pair._1, "<input>", 1, pair._2)
  implicit def tupleToInputString(tuple: Tuple3[String, Int, Int]): InputString = InputString(tuple._1, "<input>", tuple._2, tuple._3)
}