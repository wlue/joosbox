package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class AutomataSpec extends Specification {
  "NFA" should {
    "construct a valid NFA" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> Set(State("k")))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "fail to construct an NFA with an invalid start state" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> Set(State("k")))),
        State("h"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid accepting state set" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> Set(State("k")))),
        State("p"),
        Set(State("k"), State("h"))
      ) must throwA[IllegalArgumentException]
    }

    "fail to construct an NFA with an invalid state transition table" in {
      NFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> Set(State("r")))),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }

    "convert a basic instance of itself into a DFA" in {
      NFA(
        Set(State("p"), State("k"), State("t")),
        Set(Symbol("1")),
        Map(State("p") -> Map(
          Symbol("1") -> Set(State("k"))
        )),
        State("p"),
        Set(State("k"))
      ).toDFA must haveClass[DFA]
    }

    "convert a basic instance of itself with ε-transitions into a DFA" in {
      NFA(
        Set(State("1"), State("2"), State("3"), State("4"), State("5")),
        Set(Symbol("a"), Symbol("b"), Symbol.epsilon),
        Map(
          State("1") -> Map(
            Symbol("a") -> Set(State("3")),
            Symbol.epsilon -> Set(State("2"))
          ),
          State("2") -> Map(
            Symbol("a") -> Set(State("5"), State("4"))
          ),
          State("3") -> Map(
            Symbol("b") -> Set(State("4"))
          ),
          State("4") -> Map(
            Symbol("a") -> Set(State("5")),
            Symbol("b") -> Set(State("5"))
          )
        ),
        State("1"),
        Set(State("5"))
      ).toDFA must haveClass[DFA]
    }
  }

  "DFA" should {
    "construct a valid DFA" in {
      DFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Map(State("p") -> Map(Symbol("1") -> Set(State("k")))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "fail to construct a DFA with missing input symbols in its symbol set" in {
      DFA(
        Set(State("p"), State("k"), State("t")),
        Set(Symbol("1")),
        Map(State("p") -> Map(
          Symbol("1") -> Set(State("k")),
          Symbol("") -> Set(State("t"))
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
          Symbol("1") -> Set(State("k")),
          Symbol("") -> Set(State("t"))
        )),
        State("p"),
        Set(State("k"))
      ) must throwA[IllegalArgumentException]
    }
  }
}

