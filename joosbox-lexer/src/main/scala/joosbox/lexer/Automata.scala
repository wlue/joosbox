package joosbox.lexer

/**
 * Automata representing NFA/DFA.
 */
abstract class Automata(
  // Set of all of the possible states in the automata.
  val states:             Set[State],

  // Set of all possible input symbols accepted by the automata.
  val symbols:            Set[Symbol],

  // A map of maps of symbols to sets of states, like:
  //   State ->
  //     Symbol ->
  //       Set[State]
  val relation:           Relation,

  val startState:         State,

  // States that result in successful termination of the automata.
  val acceptingStates:    Set[State],

  //  Optional name.
  val name: Option[String] = None
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

  override def equals(obj: Any) = obj match {
    case automata: Automata =>
      automata.states.equals(states) &&
      automata.symbols.equals(symbols) &&
      automata.relation.equals(relation) &&
      automata.startState.equals(startState) &&
      automata.acceptingStates.equals(acceptingStates)
    case _ => false
  }

  override def toString = this.getClass.getSimpleName + "(" +
    states + ", " +
    symbols + ", " +
    relation + ", " +
    startState + ", " +
    acceptingStates + ")"

  def toGraphViz: String = {
    """digraph FiniteAutomaton {
  rankdir=LR;
  node [shape = doublecircle]; """ + acceptingStates.map{ s => "\"" + s + "\"" }.mkString(" ") + """;
  node [shape = circle];
  """ + relation.table.flatMap {
    case (start, transitions) => transitions.flatMap {
      case (symbol, states) => states.flatMap ( (state) => {
        Some("\"" + start + "\" -> \"" + state + "\" [ label = \"" + symbol + "\" ];")
      })
    }
  }.mkString("\n") + "\n}"
  }
}


/**
 * DFA
 */
object DFA {
  def apply(
    states: Set[State],
    symbols: Set[Symbol],
    relation: Relation,
    startState: State,
    acceptingStates: Set[State],
    name: Option[String] = None
  ) = new DFA(states, symbols, relation, startState, acceptingStates, name)

  def unapply(dfa: DFA) = Some((
    dfa.states, dfa.symbols, dfa.relation, dfa.startState, dfa.acceptingStates, dfa.name
  ))
}

class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  relation: Relation,
  startState: State,
  acceptingStates: Set[State],
  name: Option[String] = None
) extends Automata(states, symbols, relation, startState, acceptingStates, name) {
  if (symbols.contains(Symbol.epsilon)) {
    throw new IllegalArgumentException("DFA cannot contain epsilon transitions.")
  }


  def consume(inputString: String, state: State = startState): Option[(State, String)] = {
    if (inputString.length == 0) {
      if (acceptingStates.contains(state)) {
        Some(state, inputString)
      } else {
        None
      }
    } else {
      val result:Option[(State, String)] = relation.table.get(state).flatMap { (rules:Map[Symbol, Set[State]]) =>
        //  For each relation in the relation table, we...
        rules.keys.toList.sortWith(_.priority > _.priority).toStream.flatMap { (symbol:Symbol) =>

          //  ...check if the symbol matches our input
          if (symbol.matchSymbol(inputString.head.toString)) {
            rules.get(symbol).flatMap { (targetStates:Set[State]) =>
              targetStates.flatMap { (targetState:State) =>

                //  If it does, we move to that state and consume the rest of the input.
                consume(inputString.drop(1), targetState)
              }.headOption
            }
          } else {
            None
          }

        //  Because we're using headOption here, and we could have a bunch of relations,
        //  we convert the above sorted list to a Stream - meaning that when the first
        //  non-None Option comes through to headOption, we stop iteration, saving time.
        }.headOption
      }

      if (result == None && acceptingStates.contains(state)) {
        Some(state, inputString)
      } else {
        result
      }
    }
  }
}


/**
 * NFA
 */
object NFA {
  def apply(
    states: Set[State],
    symbols: Set[Symbol],
    relation: Relation,
    startState: State,
    acceptingStates: Set[State],
    name: Option[String] = None
  ) = new NFA(states, symbols, relation, startState, acceptingStates, name)

  def unapply(nfa: NFA) = Some((
    nfa.states, nfa.symbols, nfa.relation, nfa.startState, nfa.acceptingStates, nfa.name
  ))
}

class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  relation: Relation,
  startState: State,
  acceptingStates: Set[State],
  name: Option[String] = None
) extends Automata(states, symbols, relation, startState, acceptingStates, name) {

  /**
   * Return an identical NFA with a different name.
   */
  def withName(newName: String) = NFA(states, symbols, relation, startState, acceptingStates, Some(newName))

  /**
   * Convert the NFA to a DFA.
   */
  def toDFA: DFA = {
    // returns the set of states, set of accepting states, and relation table.
    def process(
      originalStates: Set[State],
      originalAllStates: Set[State],
      originalAcceptingStates: Set[State],
      originalRelationTable: Map[State, Map[Symbol, Set[State]]]
    ): (Set[State], Set[State], Map[State, Map[Symbol, Set[State]]]) = {
      val originalEpsilonClosure = originalStates.flatMap { s => relation.epsilonClosure(s) }
      val newState = State.combine(originalEpsilonClosure)
      val reachableStates = relation.reachableFrom(originalEpsilonClosure)

      val acceptingStates = if (originalEpsilonClosure.intersect(originalAcceptingStates).size > 0) {
        originalAcceptingStates + newState
      } else {
        originalAcceptingStates
      }

      val relationTable = if (reachableStates.size > 0) {
        originalRelationTable + (newState -> reachableStates.flatMap { transition =>
          Some(transition._1 -> Set(State.combine(transition._2.flatMap { s => relation.epsilonClosure(s) })))
        })
      } else {
        originalRelationTable
      }

      if (originalRelationTable.contains(newState)) {
        (originalAllStates + newState, acceptingStates, relationTable)
      } else {
        reachableStates.values.foldLeft((
          originalAllStates + newState,
          acceptingStates,
          relationTable
        )) { (accumulator, states) =>
          process(states, accumulator._1, accumulator._2, accumulator._3)
        }
      }
    }

    val startSet = relation.epsilonClosure(startState)
    val (newStates, newAcceptingStates, newRelationTable) = process(
      startSet, Set.empty[State], acceptingStates, Map.empty[State, Map[Symbol, Set[State]]]
    )

    DFA(
      newStates,
      symbols.filter { x => x != Symbol.epsilon },
      Relation(newRelationTable),
      State.combine(startSet),
      newAcceptingStates,
      name
    )
  }

  /**
   * TODO: Unimplemented
   */

  //  NOTE: When this does get implemented, it might be a good idea to set
  //  the matchData on each State with the name of the NFA/DFA from which
  //  that state came. That way, once a match is made on this new NFA,
  //  we can just check the matchData on the resulting accepting State
  //  and figure out the type of the token we just parsed.
  def union(that: NFA) = this
}
