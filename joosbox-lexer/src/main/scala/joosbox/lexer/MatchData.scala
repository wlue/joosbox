package joosbox.lexer

case class MatchData(kind: String, input: String = "") {
  def withInput(input: String) = MatchData(kind, input)
}
