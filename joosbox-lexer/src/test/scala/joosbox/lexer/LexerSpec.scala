import org.specs2.mutable._

import joosbox.lexer.Lexer

class LexerSpec extends Specification {
  "Lexer" should {
    "construct a valid NFA" in {
      new joosbox.lexer.NFA(
          Set(joosbox.lexer.State("p"), joosbox.lexer.State("k")),
          Set(joosbox.lexer.InputSymbol("1")),
          Map(joosbox.lexer.State("p") -> Map(joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("k"))),
          joosbox.lexer.State("p"),
          Set(joosbox.lexer.State("k"))
      )
      true
    }

    "fail to construct an NFA with an invalid start state" in {
      try {
        new joosbox.lexer.NFA(
            Set(joosbox.lexer.State("p"), joosbox.lexer.State("k")),
            Set(joosbox.lexer.InputSymbol("1")),
            Map(joosbox.lexer.State("p") -> Map(joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("k"))),
            joosbox.lexer.State("h"),
            Set(joosbox.lexer.State("k"))
        )
        false
      } catch {
        case e: IllegalArgumentException => true
      }
    }

    "fail to construct an NFA with an invalid accepting state set" in {
      try {
        new joosbox.lexer.NFA(
            Set(joosbox.lexer.State("p"), joosbox.lexer.State("k")),
            Set(joosbox.lexer.InputSymbol("1")),
            Map(joosbox.lexer.State("p") -> Map(joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("k"))),
            joosbox.lexer.State("p"),
            Set(joosbox.lexer.State("k"), joosbox.lexer.State("h"))
        )
        false
      } catch {
        case e: IllegalArgumentException => true
      }
    }

    "fail to construct an NFA with an invalid state transition table" in {
      try {
        new joosbox.lexer.NFA(
            Set(joosbox.lexer.State("p"), joosbox.lexer.State("k")),
            Set(joosbox.lexer.InputSymbol("1")),
            Map(joosbox.lexer.State("p") -> Map(joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("r"))),
            joosbox.lexer.State("p"),
            Set(joosbox.lexer.State("k"))
        )
        false
      } catch {
        case e: IllegalArgumentException => true
      }
    }

    "fail to construct a DFA with missing input symbols in its symbol set" in {
      try {
        new joosbox.lexer.DFA(
            Set(joosbox.lexer.State("p"), joosbox.lexer.State("k"), joosbox.lexer.State("t")),
            Set(joosbox.lexer.InputSymbol("1")),
            Map(joosbox.lexer.State("p") -> Map(
              joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("k"),
              joosbox.lexer.InputSymbol("") -> joosbox.lexer.State("t")
            )),
            joosbox.lexer.State("p"),
            Set(joosbox.lexer.State("k"))
        )
        false
      } catch {
        case e: IllegalArgumentException => true
      }
    }

    "fail to construct a DFA with epislon transitions" in {
      try {
        new joosbox.lexer.DFA(
            Set(joosbox.lexer.State("p"), joosbox.lexer.State("k"), joosbox.lexer.State("t")),
            Set(joosbox.lexer.InputSymbol("1"), joosbox.lexer.InputSymbol("")),
            Map(joosbox.lexer.State("p") -> Map(
              joosbox.lexer.InputSymbol("1") -> joosbox.lexer.State("k"),
              joosbox.lexer.InputSymbol("") -> joosbox.lexer.State("t")
            )),
            joosbox.lexer.State("p"),
            Set(joosbox.lexer.State("k"))
        )
        false
      } catch {
        case e: IllegalArgumentException => true
      }
    }
  }
}

