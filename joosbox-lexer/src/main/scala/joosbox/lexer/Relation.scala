package joosbox.lexer

object Relation {
  def empty: Relation = Relation(Map.empty)
}

case class Relation(table: Map[State, Map[Symbol, Set[State]]]) {
  def reachableFrom(startClosure: Set[State]): Map[Symbol, Set[State]] = {
    startClosure
      .flatMap { (state: State) =>
        table.get(state).flatMap { map: Map[Symbol, Set[State]] =>
          val out = map.flatMap { kv => kv match {
            case (Epsilon, _) => None
            case _ => Some(kv)
          }}
          if (out.size > 0) Some(out) else None
        }
      }
      .foldLeft(Map.empty[Symbol, Set[State]]) {
        case (b: Map[Symbol, Set[State]], m: Map[Symbol, Set[State]]) => {
          b ++ m.flatMap { case (symbol: Symbol, states: Set[State]) => 
              Some(symbol -> (b.getOrElse(symbol, Set.empty[State]) ++ states))
          }
        }
        case (b, x) => b
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
