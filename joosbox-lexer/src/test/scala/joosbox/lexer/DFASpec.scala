package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class DFASpec extends Specification {
  "DFA" should {
    "create" in {
      DFA(
        Set(State("p"), State("k")),
        Set(Symbol("1")),
        Relation(Map(State("p") -> Map(Symbol("1") -> Set(State("k"))))),
        State("p"),
        Set(State("k"))
      ) must not(throwA[Exception])
    }

    "unapply" in {
      val states = Set(State("p"), State("k"))
      val symbols = Set(Symbol("1"))
      val relation = Relation(Map(State("p") -> Map(Symbol("1") -> Set(State("k")))))
      val startState = State("p")
      val acceptingStates = Set(State("k"))

      val dfa = DFA(states, symbols, relation, startState, acceptingStates)
      val DFA(states2, symbols2, relation2, startState2, acceptingStates2) = dfa
      states2 must beEqualTo(states)
      symbols2 must beEqualTo(symbols)
      relation2 must beEqualTo(relation)
      startState2 must beEqualTo(startState)
      acceptingStates2 must beEqualTo(acceptingStates)
    }

    "fail to create with" in {
      "missing input symbols in its symbol set" in {
        DFA(
          Set(State("p"), State("k"), State("t")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(
            Symbol("1") -> Set(State("k")),
            Symbol("") -> Set(State("t"))
          ))),
          State("p"),
          Set(State("k"))
        ) must throwA[IllegalArgumentException]
      }

      "epislon transitions" in {
        DFA(
          Set(State("p"), State("k"), State("t")),
          Set(Symbol("1"), Symbol.epsilon),
          Relation(Map(State("p") -> Map(
            Symbol("1") -> Set(State("k")),
            Symbol("") -> Set(State("t"))
          ))),
          State("p"),
          Set(State("k"))
        ) must throwA[IllegalArgumentException]
      }
    }
  }
}
