package joosbox.lexer

/**
 * Automata representing NFA/DFA.
 */
abstract class Automata(
  // Set of all of the possible states in the automata.
  states:             Set[State],

  // Set of all possible input symbols accepted by the automata.
  symbols:            Set[Symbol],

  // A map of maps of symbols to sets of states, like:
  //   State ->
  //     Symbol ->
  //       Set[State]
  relation:           Relation,

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

  if (!relation.table.contains(startState) && !acceptingStates.contains(startState)) {
    throw new IllegalArgumentException("Transition table does not contain a transition from the start state and does not accept the start state.")
  }

  if (relation.table.keys.toSet.intersect(states).size != relation.table.keys.size) {
    throw new IllegalArgumentException("Transition table contains source states that are not found within provided states.")
  }

  val targetStates = relation.table.mapValues(v => v.values).values.flatten.flatten.toSet
  if (targetStates.intersect(states).size != targetStates.size) {
    throw new IllegalArgumentException("Transition table contains target states that are not found within provided states.")
  }

  val transitionSymbols = relation.table.values.flatMap(v => v.keys).toSet
  if (transitionSymbols.intersect(symbols).size != transitionSymbols.size) {
    throw new IllegalArgumentException("Transition table contains symbols that are not found within provided symbols.")
  }
}


/**
 * DFA
 */
object DFA {
  def apply(
    states:             Set[State],
    symbols:            Set[Symbol],
    relation:           Relation,
    startState:         State,
    acceptingStates:    Set[State]
  ) = new DFA(states, symbols, relation, startState, acceptingStates)
}

class DFA(
  states:             Set[State],
  symbols:            Set[Symbol],
  relation:           Relation,
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {
  if (symbols.contains(Symbol.epsilon)) {
    throw new IllegalArgumentException("DFA cannot cntain epsilon transitions.")
  }
}


/**
 * NFA
 */
object NFA {
  def apply(
    states:             Set[State],
    symbols:            Set[Symbol],
    relation:           Relation,
    startState:         State,
    acceptingStates:    Set[State]
  ) = new NFA(states, symbols, relation, startState, acceptingStates)
}

class NFA(
  states:             Set[State],
  symbols:            Set[Symbol],
  relation:           Relation,
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {
  def toDFA: DFA = {
    states.foreach ( state => {
      println("epsilonClosure of " + state + " == " + epsilonClosure(state))
    })

    val startClosure : Set[State] = epsilonClosure(startState)
    val startDFAState = State(startClosure.mkString(","))

    //  Every state in the values of reachableFromStart is now a new DFA state.

    DFA(states, symbols, relation, startState, acceptingStates)
  }

  def epsilonClosure(state: State) : Set[State] = {
    var worklist : Set[State] = Set(state)
    var ret : Set[State] = Set(state)

    while (worklist.size != 0) {
      val q : State = worklist.head
      worklist = worklist drop 1

      relation.table.get(q).foreach(transitions => {
        transitions.get(Symbol.epsilon).foreach(states => {
          states.foreach(qprime => {
            worklist = worklist + qprime
            ret = ret + qprime
          })
        })
      })
    }

    ret
  }
}
