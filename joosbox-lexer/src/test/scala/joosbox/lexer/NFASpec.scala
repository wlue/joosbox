package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class NFASpec extends Specification {
  "NFA" should {

    "create" in {
      NFA(
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
      val name = Some("TestName")

      val nfa = NFA(states, symbols, relation, startState, acceptingStates, name)
      val NFA(states2, symbols2, relation2, startState2, acceptingStates2, name2, stateSourceMap2) = nfa
      states2 must beEqualTo(states)
      symbols2 must beEqualTo(symbols)
      relation2 must beEqualTo(relation)
      startState2 must beEqualTo(startState)
      acceptingStates2 must beEqualTo(acceptingStates)
      name2 must beEqualTo(name)
      stateSourceMap2 must beEqualTo(nfa.stateSourceMap)
    }

    "fail to create with" in {
      "an invalid start state" in {
        NFA(
          Set(State("p"), State("k")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(Symbol("1") -> Set(State("k"))))),
          State("h"),
          Set(State("k"))
        ) must throwA[IllegalArgumentException]
      }

      "an invalid accepting state set" in {
        NFA(
          Set(State("p"), State("k")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(Symbol("1") -> Set(State("k"))))),
          State("p"),
          Set(State("k"), State("h"))
        ) must throwA[IllegalArgumentException]
      }

      "an invalid state transition table" in {
        NFA(
          Set(State("p"), State("k")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(Symbol("1") -> Set(State("r"))))),
          State("p"),
          Set(State("k"))
        ) must throwA[IllegalArgumentException]
      }
    }

    "be equal to an identical instance of itself" in {
      NFA(
        Set(State("p"), State("k"), State("t")),
        Set(Symbol("1")),
        Relation(Map(State("p") -> Map(
          Symbol("1") -> Set(State("k"))
        ))),
        State("p"),
        Set(State("k"))
      ) must beEqualTo(
        NFA(
          Set(State("p"), State("k"), State("t")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(
            Symbol("1") -> Set(State("k"))
          ))),
          State("p"),
          Set(State("k"))
        )
      )
    }

    "toDFA" in {

      "basic instance" in {
        NFA(
          Set(State("p"), State("k"), State("t")),
          Set(Symbol("1")),
          Relation(Map(State("p") -> Map(
            Symbol("1") -> Set(State("k"))
          ))),
          State("p"),
          Set(State("k"))
        ).toDFA must haveClass[DFA]
      }

      "epsilon-transitions" in {
        NFA(
          Set(State("1"), State("2"), State("3"), State("4"), State("5")),
          Set(Symbol("a"), Symbol("b"), Symbol.epsilon),
          Relation(Map(
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
          )),
          State("1"),
          Set(State("5"))
        ).toDFA must beEqualTo(
          DFA(
            Set(State("1,2"), State("3,4,5"), State("4,5"), State("5")),
            Set(Symbol("a"), Symbol("b")),
            Relation(Map(
              State("1,2") -> Map(
                Symbol("a") -> Set(State("3,4,5"))
              ),
              State("3,4,5") -> Map(
                Symbol("a") -> Set(State("5")),
                Symbol("b") -> Set(State("4,5"))
              ),
              State("4,5") -> Map(
                Symbol("a") -> Set(State("5")),
                Symbol("b") -> Set(State("5"))
              )
            )),
            State("1,2"),
            Set(State("3,4,5"), State("4,5"), State("5"))
          )
        )
      }

      "ming-ho's test case" in {
        NFA(
          Set(State("1"), State("2"), State("3"), State("4"),
              State("5"), State("6"), State("7"), State("8")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"), Symbol.epsilon),
          Relation(Map(
            State("1") -> Map(
              Symbol("a") -> Set(State("2"))
            ),
            State("2") -> Map(
              Symbol.epsilon -> Set(State("3"))
            ),
            State("3") -> Map(
              Symbol("a") -> Set(State("4"))
            ),
            State("4") -> Map(
              Symbol("a") -> Set(State("5"))
            ),
            State("5") -> Map(
              Symbol.epsilon -> Set(State("3"), State("6"))
            ),
            State("6") -> Map(
              Symbol("b") -> Set(State("6")),
              Symbol.epsilon -> Set(State("7"))
            ),
            State("7") -> Map(
              Symbol("c") -> Set(State("8"))
            )
          )),
          State("1"),
          Set(State("8"))
        ).toDFA must beEqualTo(
          DFA(
            Set(State("1"), State("2,3"), State("4"),
                State("3,5,6,7"), State("6,7"), State("8")),
            Set(Symbol("a"), Symbol("b"), Symbol("c")),
            Relation(Map(
              State("1") -> Map(
                Symbol("a") -> Set(State("2,3"))
              ),
              State("2,3") -> Map(
                Symbol("a") -> Set(State("4"))
              ),
              State("4") -> Map(
                Symbol("a") -> Set(State("3,5,6,7"))
              ),
              State("3,5,6,7") -> Map(
                Symbol("a") -> Set(State("4")),
                Symbol("b") -> Set(State("6,7")),
                Symbol("c") -> Set(State("8"))
              ),
              State("6,7") -> Map(
                Symbol("b") -> Set(State("6,7")),
                Symbol("c") -> Set(State("8"))
              )
            )),
            State("1"),
            Set(State("8"))
          )
        )
      }
    }

    "union" in {
      "simple case" in {
        val first = NFA(
          Set(State("1")),
          Set.empty,
          Relation.empty,
          State("1"),
          Set(State("1")),
          Some("A")
        )
        val second = first.withName("B")
        val combined = NFA(
          Set(State("A-B"), State("A-1"), State("B-1")),
          Set(Symbol.epsilon),
          Relation(Map(
            State("A-B") -> Map(
              Symbol.epsilon -> Set(State("A-1"), State("B-1"))
            )
          )),
          State("A-B"),
          Set(State("A-1"), State("B-1")),
          Some("AB"),
          Map(
            State("A-1") -> MatchData("A"),
            State("B-1") -> MatchData("B")
          )
        )

        first.union(second) must beEqualTo(combined)
      }
    }
  }
}
