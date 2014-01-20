package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class LexerSpec extends Specification {
  "Lexer" should {
    "construct a valid NFA" in {
      new NFA(
        Set(State("p"), State("k")),
        Set(InputSymbol("1")),
        Map(State("p") -> Map(InputSymbol("1") -> State("k"))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "fail to construct an NFA with an invalid start state" in {
      new NFA(
        Set(State("p"), State("k")),
        Set(InputSymbol("1")),
        Map(State("p") -> Map(InputSymbol("1") -> State("k"))),
        State("h"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid accepting state set" in {
      new NFA(
        Set(State("p"), State("k")),
        Set(InputSymbol("1")),
        Map(State("p") -> Map(InputSymbol("1") -> State("k"))),
        State("p"),
        Set(State("k"), State("h"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid state transition table" in {
      new NFA(
        Set(State("p"), State("k")),
        Set(InputSymbol("1")),
        Map(State("p") -> Map(InputSymbol("1") -> State("r"))),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct a DFA with missing input symbols in its symbol set" in {
      new DFA(
        Set(State("p"), State("k"), State("t")),
        Set(InputSymbol("1")),
        Map(State("p") -> Map(
          InputSymbol("1") -> State("k"),
          InputSymbol("") -> State("t")
        )),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct a DFA with epislon transitions" in {
      new DFA(
        Set(State("p"), State("k"), State("t")),
        Set(InputSymbol("1"), InputSymbol("")),
        Map(State("p") -> Map(
          InputSymbol("1") -> State("k"),
          InputSymbol("") -> State("t")
        )),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }
  }
}

