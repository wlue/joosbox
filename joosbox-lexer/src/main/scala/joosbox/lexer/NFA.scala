package joosbox.lexer

object NFA {
  def apply(
    states:             Set[State],
    symbols:            Set[Symbol],
    relation:           Map[State, Map[Symbol, State]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new NFA(states, symbols, relation, startState, acceptingStates)
}

class NFA(
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
