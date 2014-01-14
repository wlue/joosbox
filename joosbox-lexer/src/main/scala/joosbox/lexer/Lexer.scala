package joosbox.lexer

object Lexer {
  def main(args: Array[String]) {
    println("Hello, world!")

    val nfa = new NFA(
        Set(State("p"), State("k")),
        Set(InputSymbol("1")),
        (state: State, symbol: InputSymbol) => State("p"),
        State("p"),
        Set(State("k"))
    )
    println("NFA construction passed.")

    try {
      val failing = new NFA(
          Set(State("p"), State("k")),
          Set(InputSymbol("1")),
          (state: State, symbol: InputSymbol) => State("p"),
          State("h"),
          Set(State("k"))
      )
    } catch {
      case e: IllegalArgumentException => println("Failing NFA failed as expected.")
    }

    try {
      val failing = new NFA(
          Set(State("p"), State("k")),
          Set(InputSymbol("1")),
          (state: State, symbol: InputSymbol) => State("p"),
          State("p"),
          Set(State("k"), State("h"))
      )
    } catch {
      case e: IllegalArgumentException => println("Failing NFA failed as expected.")
    }
  }
}

