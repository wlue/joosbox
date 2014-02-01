package joosbox.parser 

import scala.util.control.Exception._

class ProductionRule(val nonTerminal: String, val others: List[String]) {
  override def toString: String = nonTerminal + " " + others.mkString(" ")
}

abstract class Transition(symbol: String) {

}

class ShiftTransition(symbol: String, shiftToState: Int) extends Transition(symbol) {

}

class ReduceTransition(terminal: String, rule: ProductionRule) extends Transition(terminal) {

}

object Parser {
  def fromLR1Definition(definition: String): Parser = {
    val lines:Array[String] = definition.split("\n")

    val numTerminalSymbols:Int = lines(0).toInt
    val terminalSymbols:Set[String] = lines.slice(1, 1 + numTerminalSymbols).toSet

    val numNonTerminalSymbols:Int = lines(numTerminalSymbols + 1).toInt
    val nonTerminalSymbols:Set[String] =
      lines.slice(numTerminalSymbols + 2,
                  numTerminalSymbols + 2 + numNonTerminalSymbols).toSet

    if (nonTerminalSymbols.intersect(terminalSymbols).size > 0) {
      throw new IllegalArgumentException("Symbols shared between nonterminals and terminals!")
    }

    val startSymbol:String = lines(numTerminalSymbols + numNonTerminalSymbols + 2)
    if (!nonTerminalSymbols.contains(startSymbol)) {
      throw new IllegalArgumentException("Start symbol not contained within nonterminals.")
    }

    val numProductionRules:Int = lines(numTerminalSymbols + numNonTerminalSymbols + 3).toInt
    val productionRuleStrings:List[String] =
      lines.slice(numTerminalSymbols + numNonTerminalSymbols + 4,
                  numTerminalSymbols + numNonTerminalSymbols + 4 + numProductionRules).toList

    val productionRules:List[ProductionRule] = productionRuleStrings.map {
      (ruleString: String) => {
        val tokens: Array[String] = ruleString.split(" ")
        val nonTerminal: String = tokens.head
        if (!nonTerminalSymbols.contains(nonTerminal)) {
          throw new IllegalArgumentException("Nonterminal on left side of production rule is not in nonterminal symbol set.")
        }

        val otherTokens: Array[String] = tokens.drop(1)
        if ((nonTerminalSymbols ++ terminalSymbols).intersect(otherTokens.toSet).size < otherTokens.toSet.size) {
          throw new IllegalArgumentException("Tokens on right side of production rule is not in symbol sets.")
        }

        new ProductionRule(nonTerminal, otherTokens.toList)
      }
    }

    val numStates:Int = lines(numTerminalSymbols + numNonTerminalSymbols + 4 + numProductionRules).toInt
    val numTransitions:Int = lines(numTerminalSymbols + numNonTerminalSymbols + 5 + numProductionRules).toInt
    val transitionStrings:Array[String] =
      lines.slice(numTerminalSymbols + numNonTerminalSymbols + 5 + numProductionRules + 1, lines.size)

    val transitionTable:Map[Int, Set[Transition]] = transitionStrings.foldLeft(Map.empty[Int, Set[Transition]]) {
      case (map: Map[Int, Set[Transition]], transitionString: String) => {
        val segments:Array[String] = transitionString.split(" ")
        if (segments.size == 4) {
          val (sourceStateString, symbol, action, targetString) = (segments(0), segments(1), segments(2), segments(3))
            
          val sourceState:Int = sourceStateString.toInt
          val target:Int = targetString.toInt
          val transition:Transition = action match {
            case "shift" => new ShiftTransition(symbol, target)
            case "reduce" => new ReduceTransition(symbol, productionRules(target))
          }

          map + (sourceState -> (map.getOrElse(sourceState, Set.empty[Transition]) + transition))
        } else {
          map
        }
      }
    }

    new Parser(terminalSymbols, nonTerminalSymbols, startSymbol, productionRules, transitionTable)
  }
}

class Parser(
  val terminalSymbols: Set[String],
  val nonTerminalSymbols: Set[String],
  val startSymbol: String,
  val productionRules: List[ProductionRule],
  val transitionTable: Map[Int, Set[Transition]]
) {

}
