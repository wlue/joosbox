package joosbox.lexer

object DFA {
  def apply(
    states:             Set[State],
    symbols:            Set[InputSymbol],
    relation:           Map[State, Map[InputSymbol, State]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new DFA(states, symbols, relation, startState, acceptingStates)
}

class DFA(
  states:             Set[State],
  symbols:            Set[InputSymbol],
  relation:           Map[State, Map[InputSymbol, State]],
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {
  if (symbols.contains(InputSymbol(""))) {
    throw new IllegalArgumentException("")
  }
}
