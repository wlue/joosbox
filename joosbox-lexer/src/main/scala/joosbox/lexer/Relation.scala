package joosbox.lexer

object Relation {
  def empty: Relation = Relation(Map.empty)
}

case class Relation(table: Map[State, Map[Symbol, Set[State]]]) {
  def reachableFrom(startClosure: Set[State]) = {
    startClosure
      .flatMap { (state: State) => {
        table.get(state).flatMap { map =>
            val out = map.flatMap { e => if (e._1 == Epsilon) None else Some(e) }
            if (out.size > 0) Some(out) else None
          }
        }
      }
      .foldLeft(Map.empty[Symbol, Set[State]]) {
        case (b, m: Map[Symbol, Set[State]]) => m.flatMap { e =>
          b updated (e._1, b.getOrElse(e._1, Set.empty[State]) ++ e._2)
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
