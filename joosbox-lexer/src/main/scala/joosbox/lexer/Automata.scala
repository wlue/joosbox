package joosbox.lexer

class Automata(
  // Set of all of the possible states in the automata.
  states:             Set[State],

  // Set of all possible input symbols accepted by the automata.
  symbols:            Set[InputSymbol],

  // A map of maps, like:
  //   StartState ->
  //     InputSymbol -> State
  relation:           Map[State, Map[InputSymbol, State]],

  startState:         State,

  // States that result in successful termination of the automata.
  acceptingStates:    Set[State]
) {
  if (!states.contains(startState)) {
    throw new IllegalArgumentException("Start state is not contained within provided states.")
  }

  if (acceptingStates.intersect(states).size != acceptingStates.size) {
    throw new IllegalArgumentException("Accepting states contain states that are not found within provided states.")
  }

  if (!relation.contains(startState) && !acceptingStates.contains(startState)) {
    throw new IllegalArgumentException("Transition table does not contain a transition from the start state and does not accept the start state.")
  }

  if (relation.keys.toSet.intersect(states).size != relation.keys.size) {
    throw new IllegalArgumentException("Transition table contains source states that are not found within provided states.")
  }

  val targetStates = relation.mapValues(v => v.values).values.flatten.toSet
  if (targetStates.intersect(states).size != targetStates.size) {
    throw new IllegalArgumentException("Transition table contains target states that are not found within provided states.")
  }

  val transitionSymbols = relation.values.flatMap(v => v.keys).toSet
  if (transitionSymbols.intersect(symbols).size != transitionSymbols.size) {
    throw new IllegalArgumentException("Transition table contains symbols that are not found within provided symbols.")        
  }
}
