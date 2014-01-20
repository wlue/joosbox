package joosbox.lexer

object NFA {
  def apply(
    states:             Set[State],
    symbols:            Set[InputSymbol],
    relation:           Map[State, Map[InputSymbol, State]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new NFA(states, symbols, relation, startState, acceptingStates)
}

class NFA(
  states:             Set[State],
  symbols:            Set[InputSymbol],
  relation:           Map[State, Map[InputSymbol, State]],
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {

}
