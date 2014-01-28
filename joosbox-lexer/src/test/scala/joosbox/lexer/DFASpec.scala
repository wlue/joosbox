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
      val DFA(states2, symbols2, relation2, startState2, acceptingStates2, name2) = dfa
      states2 must beEqualTo(states)
      symbols2 must beEqualTo(symbols)
      relation2 must beEqualTo(relation)
      startState2 must beEqualTo(startState)
      acceptingStates2 must beEqualTo(acceptingStates)
      name2 must beNone
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

    "consume strings" in {
      "passing simple string" in {
        DFA(
          Set(State("start"), State("gotA"), State("gotB"), State("gotC"), State("gotD")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(Symbol("b") -> Set(State("gotB"))),
            State("gotB") -> Map(Symbol("c") -> Set(State("gotC"))),
            State("gotC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("abcd") must beEqualTo(Some(State("gotD"), ""))
      }

      "failing simple string" in {
        DFA(
          Set(State("start"), State("gotA"), State("gotB"), State("gotC"), State("gotD")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(Symbol("b") -> Set(State("gotB"))),
            State("gotB") -> Map(Symbol("c") -> Set(State("gotC"))),
            State("gotC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("dcba") must beEqualTo(None)
      }

      "maximal munch" in {
        DFA(
          Set(State("start"), State("gotA"), State("gotB"), State("gotC"), State("gotD")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(Symbol("b") -> Set(State("gotB"))),
            State("gotB") -> Map(Symbol("c") -> Set(State("gotC"))),
            State("gotC") -> Map(Symbol("d") -> Set(State("gotD"))),
            State("gotD") -> Map(Symbol("a") -> Set(State("gotA")))
          )),
          State("start"),
          Set(State("gotB"), State("gotD"))
        ).consume("abcdabcd") must beEqualTo(Some(State("gotD"), ""))
      }

      "maximal munch if all states accept" in {
        DFA(
          Set(State("start"), State("gotA"), State("gotB"), State("gotC"), State("gotD")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(Symbol("b") -> Set(State("gotB"))),
            State("gotB") -> Map(Symbol("c") -> Set(State("gotC"))),
            State("gotC") -> Map(Symbol("d") -> Set(State("gotD"))),
            State("gotD") -> Map(Symbol("a") -> Set(State("gotA")))
          )),
          State("start"),
          Set(State("start"), State("gotA"), State("gotB"), State("gotC"), State("gotD"))
        ).consume("abcdabcd") must beEqualTo(Some(State("gotD"), ""))
      }

      "maximal munch if extra input exists" in {
        DFA(
          Set(State("start"), State("a"), State("aa")),
          Set(Symbol("a")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("a"))),
            State("a") -> Map(Symbol("a") -> Set(State("aa")))
          )),
          State("start"),
          Set(State("aa"), State("a"))
        ).consume("aaa") must beEqualTo(Some(State("aa"), "a"))
      }

      "maximal munch with rewinding" in {
        DFA(
          Set(State("start"), State("a"), State("ab"), State("abc"), State("b")),
          Set(Symbol("a"), Symbol("b"), Symbol("c")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("a"))),
            State("a") -> Map(Symbol("b") -> Set(State("ab"))),
            State("ab") -> Map(Symbol("c") -> Set(State("abc")))
          )),
          State("start"),
          Set(State("a"), State("b"), State("abc"))
        ).consume("ababc") must beEqualTo(Some(State("a"), "babc"))
      }

      "passing simple string using negated symbols" in {
        DFA(
          Set(State("start"), State("gotA"), State("notB"), State("notC"), State("gotD")),
          Set(Symbol("a"), NegatedSymbols("b"), NegatedSymbols("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(NegatedSymbols("b") -> Set(State("notB"))),
            State("notB") -> Map(NegatedSymbols("c") -> Set(State("notC"))),
            State("notC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("acbd") must beEqualTo(Some(State("gotD"), ""))
      }

      "failing simple string using negated symbols" in {
        DFA(
          Set(State("start"), State("gotA"), State("notB"), State("notC"), State("gotD")),
          Set(Symbol("a"), NegatedSymbols("b"), NegatedSymbols("c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(NegatedSymbols("b") -> Set(State("notB"))),
            State("notB") -> Map(NegatedSymbols("c") -> Set(State("notC"))),
            State("notC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("abcd") must beEqualTo(None)
      }

      "passing simple string using more negated symbols" in {
        DFA(
          Set(State("start"), State("gotA"), State("notBorC"), State("gotD")),
          Set(Symbol("a"), NegatedSymbols("b", "c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(NegatedSymbols("b", "c") -> Set(State("notBorC"))),
            State("notBorC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("add") must beEqualTo(Some(State("gotD"), ""))
      }

      "failing simple string using more negated symbols" in {
        DFA(
          Set(State("start"), State("gotA"), State("notBorC"), State("gotD")),
          Set(Symbol("a"), NegatedSymbols("b", "c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(NegatedSymbols("b", "c") -> Set(State("notBorC"))),
            State("notBorC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("abd") must beEqualTo(None)
      }

      "failing simple string using more negated symbols" in {
        DFA(
          Set(State("start"), State("gotA"), State("notBorC"), State("gotD")),
          Set(Symbol("a"), NegatedSymbols("b", "c"), Symbol("d")),
          Relation(Map(
            State("start") -> Map(Symbol("a") -> Set(State("gotA"))),
            State("gotA") -> Map(NegatedSymbols("b", "c") -> Set(State("notBorC"))),
            State("notBorC") -> Map(Symbol("d") -> Set(State("gotD")))
          )),
          State("start"),
          Set(State("gotD"))
        ).consume("acd") must beEqualTo(None)
      }
    }

    "match" in {
      "test basic NFA" in {
        val dfa = NFA(
          Set(State("a"), State("b"), State("c", Some(MatchData("c"))), State("x"),
              State("y"), State("z", Some(MatchData("z"))), State("start")),
          Set(Symbol("a"), Symbol("b"), Symbol("c"),
              Symbol("x"), Symbol("y"), Symbol("z")),
          Relation(Map(
            State("start") -> Map(
              Symbol("a") -> Set(State("a")),
              Symbol("x") -> Set(State("x"))
            ),
            State("a") -> Map(
              Symbol("b") -> Set(State("b"))
            ),
            State("b") -> Map(
              //  Note - this MatchData is what is currently returned.
              //  This should be refactored to store matchData outside the state itself.
              Symbol("c") -> Set(State("c", Some(MatchData("c"))))
            ),
            State("x") -> Map(
              Symbol("y") -> Set(State("y"))
            ),
            State("y") -> Map(
              Symbol("z") -> Set(State("z", Some(MatchData("z"))))
            )
          )),
          State("start"),
          Set(
            State("c", Some(MatchData("c"))),
            State("z", Some(MatchData("z")))
          )
        ).toDFA

        dfa.matchString("abc") must beEqualTo(List(MatchData("c")))
        dfa.matchString("abcdef") must beEqualTo(List(MatchData("c")))
        dfa.matchString("abcdefghi") must beEqualTo(List(MatchData("c")))
        dfa.matchString("abcxyz") must beEqualTo(List(MatchData("c"), MatchData("z")))

        dfa.consume("abc") must beEqualTo(Some(State("c"), ""))
        dfa.consume("abcdef") must beEqualTo(Some(State("c"), "def"))
        dfa.consume("xyz") must beEqualTo(Some(State("z"), ""))
        dfa.consume("xyz123") must beEqualTo(Some(State("z"), "123"))
      }
    }
  }
}
