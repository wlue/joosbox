package joosbox.lexer

case class Relation(table: Map[State, Map[Symbol, Set[State]]]) {
    def reachableFrom(startClosure: Set[State]) = startClosure
      .flatMap { (state: State) => table.get(state) }
      .flatten
      .foldLeft(Map[Symbol, Set[State]]().withDefaultValue(Set[State]())) {

        //  If we have an input symbol that gets us to another state,
        //  add it to our map.
        case (b, (x: InputSymbol, y: Set[State])) => b updated (x, b(x) ++ y)

        //  Epsilon transitions are already included in reachableFromStart,
        //  so just ignore  them here.
        case (b, _) => b
      }
}