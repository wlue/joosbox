package joosbox.lexer

class NFA(
  states:             scala.collection.Set[State],
  symbols:            scala.collection.Set[InputSymbol],
  relation:           (State, InputSymbol) => State,
  startState:         State,
  acceptingStates:    scala.collection.Set[State]
  ) {

  if (!states.contains(startState)) {
    throw new IllegalArgumentException("Start state is not contained within provided states.")
  }

  if (acceptingStates.intersect(states).size != acceptingStates.size) {
    throw new IllegalArgumentException("Accepting states contain states that are not found within provided states.")
  }
}