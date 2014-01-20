package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class AutomataSpec extends Specification {
  "NFA" should {
    "construct a valid NFA" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> State("k"))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "fail to construct an NFA with an invalid start state" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> State("k"))),
        State("h"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid accepting state set" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> State("k"))),
        State("p"),
        Set(State("k"), State("h"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid state transition table" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> State("r"))),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }
  }

  "DFA" should {
    "construct a valid DFA" in {
      DFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> State("k"))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "fail to construct a DFA with missing input symbols in its symbol set" in {
      DFA(
        Set(State("p"), State("k"), State("t")),
        Set(Symbol("1")),
        Map(State("p") -> Map(
          Symbol("1") -> State("k"),
          Symbol("") -> State("t")
        )),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct a DFA with epislon transitions" in {
      DFA(
        Set(State("p"), State("k"), State("t")),
        Set(Symbol("1"), Symbol.epsilon),
        Map(State("p") -> Map(
          Symbol("1") -> State("k"),
          Symbol("") -> State("t")
        )),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }
  }
}

