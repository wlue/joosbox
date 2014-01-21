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
  relation:           Map[State, Map[Symbol, Set[State]]],

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

  val targetStates = relation.mapValues(v => v.values).values.flatten.flatten.toSet
  if (targetStates.intersect(states).size != targetStates.size) {
    throw new IllegalArgumentException("Transition table contains target states that are not found within provided states.")
  }

  val transitionSymbols = relation.values.flatMap(v => v.keys).toSet
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
    relation:           Map[State, Map[Symbol, Set[State]]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new DFA(states, symbols, relation, startState, acceptingStates)
}

class DFA(
  states:             Set[State],
  symbols:            Set[Symbol],
  relation:           Map[State, Map[Symbol, Set[State]]],
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
    relation:           Map[State, Map[Symbol, Set[State]]],
    startState:         State,
    acceptingStates:    Set[State]
  ) = new NFA(states, symbols, relation, startState, acceptingStates)
}

class NFA(
  states:             Set[State],
  symbols:            Set[Symbol],
  relation:           Map[State, Map[Symbol, Set[State]]],
  startState:         State,
  acceptingStates:    Set[State]
) extends Automata(states, symbols, relation, startState, acceptingStates) {
  def toDFA() : DFA = {
    states.foreach ( state => {
      println("epsilonClosure of " + state + " == " + epsilonClosure(state))
    })

    val startClosure : Set[State] = epsilonClosure(startState)
    val startDFAState = State(startClosure.mkString(","))

    val reachableFromStart = startClosure.flatMap(
      (state) => relation get state
    ).flatten.foldLeft(Map[Symbol, Set[State]]().withDefaultValue(Set[State]())){

      //  If we have an input symbol that gets us to another state,
      //  add it to our map.
      case (b, (x: InputSymbol, y: Set[State])) => b updated (x, b(x) ++ y)

      //  Epsilon transitions are already included in reachableFromStart,
      //  so just ignore  them here.
      case (b, _) => b
    }

    println(reachableFromStart)

    //  Every state in the values of reachableFromStart is now a new DFA state.

    DFA(
      states, symbols, relation, startState, acceptingStates
    )
  }

  def epsilonClosure(state: State) : Set[State] = {
    var worklist : Set[State] = Set(state)
    var ret : Set[State] = Set(state)

    while (worklist.size != 0) {
      val q : State = worklist.head
      worklist = worklist drop 1

      relation.get(q).foreach(transitions => {
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
