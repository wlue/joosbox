package joosbox.lexer

class Automata(
  //  Set of all of the possible states in the automata.
  states:             scala.collection.Set[State],

  //  Set of all possible input symbols accepted by the automata.
  symbols:            scala.collection.Set[InputSymbol],

  //  A map of maps, like:
  //    StartState ->
  //      InputSymbol -> State
  relation:           scala.collection.Map[State, scala.collection.Map[InputSymbol, State]],

  startState:         State,

  //  States that result in successful termination of the automata.
  acceptingStates:    scala.collection.Set[State]
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

  val target_states = relation.mapValues(v => v.values).values.flatten.toSet
  if (target_states.intersect(states).size != target_states.size) {
    throw new IllegalArgumentException("Transition table contains target states that are not found within provided states.")        
  }

  val transition_symbols = relation.values.flatMap(v => v.keys).toSet
  if (transition_symbols.intersect(symbols).size != transition_symbols.size) {
    throw new IllegalArgumentException("Transition table contains symbols that are not found within provided symbols.")        
  }
}

class NFA(
  states:             scala.collection.Set[State],
  symbols:            scala.collection.Set[InputSymbol],
  relation:           scala.collection.Map[State, scala.collection.Map[InputSymbol, State]],
  startState:         State,
  acceptingStates:    scala.collection.Set[State])
extends Automata(states, symbols, relation, startState, acceptingStates) {

}

class DFA(
  states:             scala.collection.Set[State],
  symbols:            scala.collection.Set[InputSymbol],
  relation:           scala.collection.Map[State, scala.collection.Map[InputSymbol, State]],
  startState:         State,
  acceptingStates:    scala.collection.Set[State])
extends Automata(states, symbols, relation, startState, acceptingStates) {
  if (symbols.contains(InputSymbol(""))) {
    throw new IllegalArgumentException("")        
  }
}