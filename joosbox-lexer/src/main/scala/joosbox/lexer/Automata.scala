package joosbox.lexer

import org.apache.commons.lang.StringEscapeUtils.escapeJava

/**
 * Automata representing NFA/DFA.
 */
abstract class Automata(
  // Set of all of the possible states in the automata.
  val states: Set[State],

  // Set of all possible input symbols accepted by the automata.
  val symbols: Set[Symbol],

  // A map of maps of symbols to sets of states, like:
  //   State ->
  //     Symbol ->
  //       Set[State]
  val relation: Relation,

  val startState: State,

  // States that result in successful termination of the automata.
  val acceptingStates: Set[State],

  // Optional name.
  val token: Option[TokenType] = None,

  // Map from states to match data.
  var _stateSourceMap: Map[State, TokenType] = Map.empty[State, TokenType]
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

  if (_stateSourceMap.size == 0 && token != None) {
    _stateSourceMap = states.map { state: State => (state, token.get) }.toMap
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
      automata.token.equals(token) &&
      automata.stateSourceMap.equals(stateSourceMap)
    case _ => false
  }

  override def toString = this.getClass.getSimpleName + "(" +
    states + ", " +
    symbols + ", " +
    relation + ", " +
    startState + ", " +
    acceptingStates + "," + 
    token + "," +
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
    token: Option[TokenType] = None,
    _stateSourceMap: Map[State, TokenType] = Map.empty[State, TokenType]
  ) = new DFA(states, symbols, relation, startState, acceptingStates, token, _stateSourceMap)

  def unapply(dfa: DFA) = Some((
    dfa.states, dfa.symbols, dfa.relation, dfa.startState, dfa.acceptingStates, dfa.token, dfa.stateSourceMap
  ))
}

class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  relation: Relation,
  startState: State,
  acceptingStates: Set[State],
  token: Option[TokenType] = None,
  _stateSourceMap: Map[State, TokenType] = Map.empty[State, TokenType]
) extends Automata(states, symbols, relation, startState, acceptingStates, token, _stateSourceMap) {
  if (symbols.contains(Symbol.epsilon)) {
    throw new IllegalArgumentException("DFA cannot contain epsilon transitions.")
  }

  final def consume(inputString: String, state: State = startState): Option[(State, String)] = {
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
              targetStates.headOption match {
                case Some(targetState: State) =>
                  //  If it does, we move to that state and consume the rest of the input.  
                  consume(inputString.drop(1), targetState)
                case None => None
              }
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

  def matchString(inputString: String, filename: String = "<input>", line: Int = 1, idx: Int = 0): Option[List[Token]] = {
    consume(inputString, startState)
      .flatMap[List[Token]] { case (state: State, remainingString: String) =>
        if (remainingString.length == 0) {
          val tokenType: TokenType = stateSourceMap(state)
          val token: Token = tokenType.create(InputString(inputString, filename, line, idx))
          Some(List(token))
        } else {
          val consumedInput: String = inputString.slice(0, inputString.size - remainingString.size)

          val newLine: Int = consumedInput.foldLeft(line) { (x: Int, c: Char) =>
            if (c == '\n') x + 1 else x
          }

          val newIdx: Int = consumedInput.foldLeft(idx) { (x: Int, c: Char) =>
            if (c == '\n') 0 else x + 1
          }

          matchString(remainingString, filename, newLine, newIdx) match {
            case Some(remainingTokens) =>
              val tokenType: TokenType = stateSourceMap(state)
              val token: Token = tokenType.create(InputString(consumedInput, filename, line, idx))
              Some(List(token) ++ remainingTokens)
            case None => throw new SyntaxError("Input invalid at " + filename + ":" + line + ":" + idx)
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
    token: Option[TokenType] = None,
    _stateSourceMap: Map[State, TokenType] = Map.empty[State, TokenType]
  ) = new NFA(states, symbols, relation, startState, acceptingStates, token, _stateSourceMap)

  def unapply(nfa: NFA) = Some((
    nfa.states, nfa.symbols, nfa.relation, nfa.startState, nfa.acceptingStates, nfa.token, nfa.stateSourceMap
  ))

  def fromString(inputString: String, token: TokenType) = {
    val prefixes: Set[State] = (1 to inputString.size).map(inputString.slice(0, _)).map(State(_)).toSet
    val states: Set[State] = prefixes + State("start")
    val symbols: Set[Symbol] = inputString.toSet[Char].map(_.toString).map(InputSymbol(_))
    val table: Map[State, Map[Symbol, Set[State]]] =
      inputString
        .zipWithIndex
        .drop(1)
        .foldLeft(Map.empty[State, Map[Symbol, Set[State]]]) { case (table, pair) =>
          val (c: Char, index: Int) = pair
          val fromState: State = State(inputString.slice(0, index))
          val toState: State = State(inputString.slice(0, index + 1))
          val symbol: Symbol = Symbol(c.toString)

          val map: Map[Symbol, Set[State]] = table.getOrElse(fromState, Map.empty[Symbol, Set[State]])
          val newMap: Map[Symbol, Set[State]] = map + (symbol -> Set(toState))

          table + (fromState -> newMap)
        } + (
          State("start") -> Map(Symbol(inputString.head.toString) -> Set(State(inputString.head.toString)))
        )
    val startState: State = State("start")
    val acceptingStates: Set[State] = Set(State(inputString))
    val stateSourceMap: Map[State, TokenType] = states.map { s: State => (s -> token) }.toMap

    NFA(states, symbols, Relation(table), startState, acceptingStates, Some(token), stateSourceMap)
  }

  def union(nfas: Set[NFA]): NFA = {
    val prefixedNFAs: Set[NFA] = nfas.map(_.toPrefixedForm)

    //  TODO: If there is no name here, generate one.
    val newName: String = prefixedNFAs.flatMap(_.token).map(_.name).mkString("|")
    val newStartState: State = State(newName)

    val newAcceptingStates: Set[State] = prefixedNFAs.map(_.acceptingStates).reduce(_ ++ _)
    val newRelationTable: Map[State, Map[Symbol, Set[State]]] = {
      val existingRelations = prefixedNFAs.map(_.relation.table).reduce(_ ++ _)
      val startStates: Set[State] = prefixedNFAs.map(_.startState)
      existingRelations + (newStartState -> Map(Symbol.epsilon -> startStates))
    }

    val newStateSourceMap: Map[State, TokenType] = prefixedNFAs.map(_.stateSourceMap).reduce(_ ++ _)
    val newStates: Set[State] = prefixedNFAs.map(_.states).reduce(_ ++ _) + newStartState
    val newSymbols: Set[Symbol] = prefixedNFAs.map(_.symbols).reduce(_ ++ _) + Symbol.epsilon

    NFA(
      newStates,
      newSymbols,
      Relation(newRelationTable),
      newStartState,
      newAcceptingStates,
      Some(CombinedTokenType.createFrom(newName)),
      newStateSourceMap
    )
  }
}

class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  relation: Relation,
  startState: State,
  acceptingStates: Set[State],
  token: Option[TokenType] = None,
  _stateSourceMap: Map[State, TokenType] = Map.empty[State, TokenType]
) extends Automata(states, symbols, relation, startState, acceptingStates, token, _stateSourceMap) {

  /**
   * Return an identical NFA with a different name.
   */
  def withToken(newKind: TokenType): NFA = {
    val renamedStateSourceMap = stateSourceMap.map {
      case (state: State, data: TokenType) => data match {
        case _: TokenType => (state -> newKind)
        case _ => (state -> data)
      }
    }
    NFA(states, symbols, relation, startState, acceptingStates, Some(newKind), renamedStateSourceMap)
  }

  /**
   * Convert the NFA to a DFA.
   */
  def toDFA: DFA = {
    // Returns the set of states, set of accepting states, and relation table.
    def process(
      originalStates: Set[State],
      originalAllStates: Set[State],
      originalAcceptingStates: Set[State],
      originalRelationTable: Map[State, Map[Symbol, Set[State]]],
      originalStateSourceMap: Map[State, TokenType]
    ): (Set[State], Set[State], Map[State, Map[Symbol, Set[State]]], Map[State, TokenType]) = {
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

      val stateSourceMap: Map[State, TokenType] = {
        val sourceStates: Set[State] =
          originalEpsilonClosure
            .intersect(originalStateSourceMap.keys.toSet)
            .intersect(acceptingStates)

        if (sourceStates.size > 0) {
          val possibleTokens: Set[TokenType] = sourceStates.map(originalStateSourceMap(_))
          val highestPriority: TokenType =
            possibleTokens
              .toList
              .sortWith(_.priority < _.priority)
              .head

          originalStateSourceMap + (newState -> highestPriority)
        } else {
          originalStateSourceMap
        }
      }

      val relationTable: Map[State, Map[Symbol, Set[State]]] =
        if (reachableStates.size > 0) {
          val value: Map[Symbol, Set[State]] =
            reachableStates.map { case (symbol: Symbol, states: Set[State]) =>
              val combinedStates: Set[State] = states.flatMap { (state: State) =>
                relation.epsilonClosure(state)
              }
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

    val prunedStateSourceMap = newStateSourceMap.flatMap { case (state: State, data: TokenType) =>
      if (newStates.contains(state)) Some(state -> data) else None
    }.toMap

    val pnac = newAcceptingStates.intersect(newStates)

    DFA(
      newStates,
      symbols.filter { x => x != Symbol.epsilon },
      Relation(newRelationTable),
      State.combine(startSet),
      pnac,
      token,
      prunedStateSourceMap
    )
  }

  def toPrefixedForm: NFA = token match {
    case None => this
    case Some(name) =>
      val newStates: Set[State] = states.map(State.prefixed(name, _))
      val newAcceptingStates: Set[State] = acceptingStates.map(State.prefixed(name, _))
      val newRelationTable: Map[State, Map[Symbol, Set[State]]] =
        relation.table.map { case (state: State, transitions: Map[Symbol, Set[State]]) =>
          State.prefixed(name, state) -> transitions.map { case (symbol: Symbol, states: Set[State]) =>
            symbol -> states.map { destState: State =>
              State.prefixed(name, destState)
            }
          }
        }

      val newStateSourceMap: Map[State, TokenType] =
        stateSourceMap.map { case (state: State, data: TokenType) =>
          State.prefixed(name, state) -> data
        }

      NFA(
        newStates,
        symbols,
        Relation(newRelationTable),
        State.prefixed(name, startState),
        newAcceptingStates,
        Some(name),
        newStateSourceMap
      )
  }

  def union(that: NFA): NFA = NFA.union(Set(this, that))
  def union(nfas: Set[NFA]): NFA = NFA.union(nfas + this)
}
