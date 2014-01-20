package joosbox.lexer

object DFA {
  def apply(
    states:             Set[State],
    symbols:            Set[Symbol],
    relation:           Map[State, Map[Symbol, State]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new DFA(states, symbols, relation, startState, acceptingStates)
}

class DFA(
  states:             Set[State],
  symbols:            Set[Symbol],
  relation:           Map[State, Map[Symbol, State]],
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {
  if (symbols.contains(Symbol(""))) {
    throw new IllegalArgumentException("")
  }
}
