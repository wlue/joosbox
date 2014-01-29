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
  val name: Option[String] = None,

  //  Map from states to match data.
  var _stateSourceMap: Map[State, MatchData] = Map.empty[State, MatchData]
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

  if (_stateSourceMap.size == 0 && name != None) {
    _stateSourceMap = states.map{ case (state: State) => (state, MatchData(name.get)) }.toMap
  }

  val stateSourceMap = _stateSourceMap

  if (stateSourceMap.keys.toSet.intersect(states).size != stateSourceMap.keys.size) {
    throw new IllegalArgumentException("State source map contains states not found within provided states.")
  }

  override def equals(obj: Any) = obj match {
    case automata: Automata =>
      automata.states.equals(states) &&
      automata.symbols.equals(symbols) &&
      automata.relation.equals(relation) &&
      automata.startState.equals(startState) &&
      automata.acceptingStates.equals(acceptingStates) &&
      automata.stateSourceMap.equals(stateSourceMap)
    case _ => false
  }

  override def toString = this.getClass.getSimpleName + "(" +
    states + ", " +
    symbols + ", " +
    relation + ", " +
    startState + ", " +
    acceptingStates + "," + 
    stateSourceMap + ")"

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
    name: Option[String] = None,
    _stateSourceMap: Map[State, MatchData] = Map.empty[State, MatchData]
  ) = new DFA(states, symbols, relation, startState, acceptingStates, name, _stateSourceMap)

  def unapply(dfa: DFA) = Some((
    dfa.states, dfa.symbols, dfa.relation, dfa.startState, dfa.acceptingStates, dfa.name, dfa.stateSourceMap
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

  def matchString(inputString: String): Option[List[MatchData]] = {
    consume(inputString, startState).flatMap[List[MatchData]] {
      case (state: State, remaining: String) => {
        if (remaining.length == 0) {
          Some(List[MatchData](stateSourceMap(state)))
        } else {
          matchString(remaining) match {
            case Some(remainingMatchData) => {
              Some(List[MatchData](stateSourceMap(state)) ++ remainingMatchData)
            }
            case None => None
          }
        }
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
    name: Option[String] = None,
    _stateSourceMap: Map[State, MatchData] = Map.empty[State, MatchData]
  ) = new NFA(states, symbols, relation, startState, acceptingStates, name, _stateSourceMap)

  def unapply(nfa: NFA) = Some((
    nfa.states, nfa.symbols, nfa.relation, nfa.startState, nfa.acceptingStates, nfa.name, nfa.stateSourceMap
  ))
}

class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  relation: Relation,
  startState: State,
  acceptingStates: Set[State],
  name: Option[String] = None,
  _stateSourceMap: Map[State, MatchData] = Map.empty[State, MatchData]
) extends Automata(states, symbols, relation, startState, acceptingStates, name, _stateSourceMap) {

  /**
   * Return an identical NFA with a different name.
   */
  def withName(newName: String) = NFA(states, symbols, relation, startState, acceptingStates, Some(newName), stateSourceMap)

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
      val originalEpsilonClosure: Set[State] = originalStates.flatMap { state: State =>
        relation.epsilonClosure(state)
      }

      val newState: State = State.combine(originalEpsilonClosure)
      val reachableStates: Map[Symbol, Set[State]] = relation.reachableFrom(originalEpsilonClosure)

      val acceptingStates: Set[State] =
        if (originalEpsilonClosure.intersect(originalAcceptingStates).size > 0) {
          originalAcceptingStates + newState
        } else {
          originalAcceptingStates
        }

      val relationTable: Map[State, Map[Symbol, Set[State]]] =
        if (reachableStates.size > 0) {
          val value: Map[Symbol, Set[State]] =
            reachableStates.map { case (symbol: Symbol, states: Set[State]) =>
              val combinedStates: Set[State] = states.flatMap { state: State => relation.epsilonClosure(state) }
              symbol -> Set(State.combine(combinedStates))
            }

          originalRelationTable + (newState -> value)
        } else {
          originalRelationTable
        }

      if (originalRelationTable.contains(newState)) {
        (originalAllStates + newState, acceptingStates, relationTable)
      } else {
        val initial = (originalAllStates + newState, acceptingStates, relationTable)
        reachableStates.values.foldLeft(initial) { case (accumulator, states) =>
          process(states, accumulator._1, accumulator._2, accumulator._3)
        }
      }
    }

    val startSet = relation.epsilonClosure(startState)
    val (newStates, newAcceptingStates, newRelationTable) = process(
      startSet, Set.empty[State], acceptingStates, Map.empty[State, Map[Symbol, Set[State]]]
    )

    val newStateSourceMap = states.flatMap {
      case (state: State) => {
        val states = relation.epsilonClosure(state)
        val combinedMatchData: Set[MatchData] = states.flatMap(stateSourceMap.get(_))

        combinedMatchData.headOption match {
          case None => None
          case Some(data) => Some(State.combine(states) -> data)
        }
      }
    }.toMap

    DFA(
      newStates,
      symbols.filter { x => x != Symbol.epsilon },
      Relation(newRelationTable),
      State.combine(startSet),
      newAcceptingStates.intersect(newStates),
      name,
      newStateSourceMap
    )
  }

  /**
   * TODO: Unimplemented
   */
  // NOTE: When this does get implemented, it might be a good idea to set
  // the matchData on each State with the name of the NFA/DFA from which
  // that state came. That way, once a match is made on this new NFA,
  // we can just check the matchData on the resulting accepting State
  // and figure out the type of the token we just parsed.
  def union(that: NFA) = this
}
