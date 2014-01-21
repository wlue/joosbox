package joosbox.lexer

case class Relation(table: Map[State, Map[Symbol, Set[State]]]) {
  def reachableFrom(startClosure: Set[State]) = {
    startClosure
      .flatMap { (state: State) => table.get(state) }
      .flatten
      .foldLeft(Map.empty[Symbol, Set[State]].withDefaultValue(Set.empty[State])) {

        // If we have an input symbol that gets us to another state,
        // add it to our map.
        case (b, (x: InputSymbol, y: Set[State])) => b updated (x, b(x) ++ y)

        // Epsilon transitions are already included in reachableFromStart,
        // so just ignore  them here.
        case (b, _) => b
      }
  }

  def epsilonClosure(state: State): Set[State] = {
    var worklist: Set[State] = Set(state)
    var ret: Set[State] = Set(state)

    while (worklist.size != 0) {
      val q: State = worklist.head
      worklist = worklist.drop(1)

      table.get(q).foreach { transitions =>
        transitions
          .get(Symbol.epsilon)
          .foreach { states =>
            states.foreach { qprime =>
              worklist = worklist + qprime
              ret = ret + qprime
            }
          }
      }
    }
    ret
  }
}
