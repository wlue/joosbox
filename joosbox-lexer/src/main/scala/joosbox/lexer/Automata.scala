package joosbox.lexer

import org.apache.commons.lang.StringEscapeUtils.escapeJava

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
    throw new IllegalArgumentException(
      "State source map contains states not found within provided states. Source map:\n" +
      stateSourceMap + "\nProvided states:\n" + states + "\n"
    )
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
  node [shape = doublecircle]; """ + acceptingStates.map{ s => "\"" + s.name + "\"" }.mkString(" ") + """;
  node [shape = circle];
  """ + relation.table.flatMap {
    case (start, transitions) => transitions.flatMap {
      case (symbol, states) => states.flatMap ( (state) => {
        Some("\"" + escapeJava(start.name) + "\" -> \"" + escapeJava(state.name) + "\" [ label = \"" + escapeJava(symbol.toString) + "\" ];")
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
  name: Option[String] = None,
  _stateSourceMap: Map[State, MatchData] = Map.empty[State, MatchData]
) extends Automata(states, symbols, relation, startState, acceptingStates, name, _stateSourceMap) {
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
  def withName(newName: String) = {
    val renamedStateSourceMap = stateSourceMap.map {
      case (state: State, data: MatchData) => {
        data match {
          case MatchData(name) => state -> MatchData(newName)
          case _ => state -> data
        }
      }
    }
    NFA(states, symbols, relation, startState, acceptingStates, Some(newName), renamedStateSourceMap)
  }

  /**
   * Convert the NFA to a DFA.
   */
  def toDFA: DFA = {
    // returns the set of states, set of accepting states, and relation table.
    def process(
      originalStates: Set[State],
      originalAllStates: Set[State],
      originalAcceptingStates: Set[State],
      originalRelationTable: Map[State, Map[Symbol, Set[State]]],
      originalStateSourceMap: Map[State, MatchData]
    ): (Set[State], Set[State], Map[State, Map[Symbol, Set[State]]], Map[State, MatchData]) = {
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

      val stateSourceMap: Map[State, MatchData] = {
        val sourceMatchData: Set[State] = originalEpsilonClosure.intersect(originalStateSourceMap.keys.toSet).intersect(acceptingStates)
        if (sourceMatchData.size > 0) {
          originalStateSourceMap + (newState -> originalStateSourceMap(sourceMatchData.head))
        } else {
          originalStateSourceMap
        }
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
        (originalAllStates + newState, acceptingStates, relationTable, stateSourceMap)
      } else {
        val initial = (originalAllStates + newState, acceptingStates, relationTable, stateSourceMap)
        reachableStates.values.foldLeft(initial) { case (accumulator, states) =>
          process(states, accumulator._1, accumulator._2, accumulator._3, accumulator._4)
        }
      }
    }

    val startSet = relation.epsilonClosure(startState)
    val (newStates, newAcceptingStates, newRelationTable, newStateSourceMap) = process(
      startSet, Set.empty[State], acceptingStates, Map.empty[State, Map[Symbol, Set[State]]], stateSourceMap
    )

    val prunedStateSourceMap = newStateSourceMap.flatMap {
      case (state: State, data: MatchData) => if (newStates.contains(state)) Some(state -> data) else None
    }.toMap
    val pnac = newAcceptingStates.intersect(newStates)

    DFA(
      newStates,
      symbols.filter { x => x != Symbol.epsilon },
      Relation(newRelationTable),
      State.combine(startSet),
      pnac,
      name,
      prunedStateSourceMap
    )
  }

  def union(that: NFA): NFA = {
    val newStates: Set[State] = {
      states.map{ s:State => State(name.get + "-" + s.name) } ++
      that.states.map{ s:State => State(that.name.get + "-" + s.name) }
    }

    //  TODO: If there is no name here, generate one.
    val newName: String = name.get + "-" + that.name.get
    val newStartState: State = State(name.get + "-" + that.name.get)

    val newAcceptingStates: Set[State] = {
      acceptingStates.map{ s:State => State(name.get + "-" + s.name) } ++
      that.acceptingStates.map{ s:State => State(that.name.get + "-" + s.name) }
    }

    val newRelationTable: Map[State, Map[Symbol, Set[State]]] = {
      relation.table.map{
        case(state: State, transitions: Map[Symbol, Set[State]]) => {
          State(name.get + "-" + state.name) -> transitions.map {
            case(symbol: Symbol, states: Set[State]) => {
              symbol -> states.map { (destState: State) => 
                State(name.get + "-" + destState.name)
              }
            }
          }
        }
      } ++ 
      that.relation.table.map{
        case(state: State, transitions: Map[Symbol, Set[State]]) => {
          State(that.name.get + "-" + state.name) -> transitions.map {
            case(symbol: Symbol, states: Set[State]) => {
              symbol -> states.map { (destState: State) => 
                State(that.name.get + "-" + destState.name)
              }
            }
          }
        }
      } ++ 
      Map[State, Map[Symbol, Set[State]]](
        newStartState -> Map(Symbol.epsilon -> Set(
          State(name.get + "-" + startState.name),
          State(that.name.get + "-" + that.startState.name)
        ))
      )
    }

    val newStateSourceMap: Map[State, MatchData] = {
      stateSourceMap.map{
        case(state: State, data: MatchData) => {
          State(name.get + "-" + state.name) -> data
        }
      } ++
      that.stateSourceMap.map{
        case(state: State, data: MatchData) => {
          State(that.name.get + "-" + state.name) -> data
        }
      }
    } 

    NFA(
      newStates + newStartState,
      symbols ++ that.symbols + Symbol.epsilon,
      Relation(newRelationTable),
      newStartState,
      newAcceptingStates,
      Some(newName),
      newStateSourceMap
    )
  }
}
