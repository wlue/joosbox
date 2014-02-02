package joosbox.parser 

import scala.util.control.Exception._

class ParseNode(val kind: String, val value: Option[String], val children: List[ParseNode]) {
  override def toString: String = if (children.size > 0) {
    "{\"kind\": \"" + kind + "\", \"value\": \"" + value + "\", \"children\": [" + children.map(_.toString).mkString(", ") + "]}"
  } else {
    "{\"kind\": \"" + kind + "\", \"value\": \"" + value + "\"}"
  }

  override def equals(obj: Any) = obj match {
    case node: ParseNode => node.kind.equals(kind) && node.value.equals(value) && node.children.equals(children)
    case _ => false
  }
}

object ParseNode {
  def apply(kind: String, value:AnyRef = None): ParseNode = {
    value match {
      case (list: List[ParseNode]) => new ParseNode(kind, None, list)
      case (string: String) => new ParseNode(kind, Some(string), List.empty[ParseNode])
      case None => new ParseNode(kind, None, List.empty[ParseNode])
    }
  }
}

class ProductionRule(val nonTerminal: String, val others: List[String]) {
  override def toString: String = nonTerminal + " " + others.mkString(" ")
  def dropCount: Int = others.size
}

abstract class Transition {}

class ShiftTransition(val shiftToState: Int) extends Transition {}
object ShiftTransition {
  def unapply(t: ShiftTransition): Option[Int] = Some(t.shiftToState)
}

class ReduceTransition(val rule: ProductionRule) extends Transition {}
object ReduceTransition {
  def unapply(t: ReduceTransition): Option[ProductionRule] = Some(t.rule)
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

    val transitionTable:Map[Int, Map[String, Transition]] = transitionStrings.foldLeft(Map.empty[Int, Map[String, Transition]]) {
      case (map: Map[Int, Map[String, Transition]], transitionString: String) => {
        val segments:Array[String] = transitionString.split(" ")
        if (segments.size == 4) {
          val (sourceStateString, symbol, action, targetString) = (segments(0), segments(1), segments(2), segments(3))
            
          val sourceState:Int = sourceStateString.toInt
          val target:Int = targetString.toInt
          val transition:Transition = action match {
            case "shift" => new ShiftTransition(target)
            case "reduce" => new ReduceTransition(productionRules(target))
          }

          map + (sourceState -> (map.getOrElse(sourceState, Map.empty[String, Transition]) + (symbol -> transition)))
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
  val transitionTable: Map[Int, Map[String, Transition]]
) {
  def parse(symbols: List[String]): ParseNode = {

    var nodeStack : scala.collection.mutable.Stack[ParseNode] = scala.collection.mutable.Stack[ParseNode]()
    var stateStack : scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int]()

    nodeStack.push(ParseNode("BOF"))
    stateStack.push(transitionTable(0)("BOF") match { case ShiftTransition(x) => x })

    (symbols ++ List[String]("EOF")).foreach {
      (a: String) => {
        var reducing = true
        while (reducing) {
          transitionTable(stateStack.top).get(a) match {
            case Some(ReduceTransition(rule: ProductionRule)) => {
              val temp : scala.collection.mutable.Stack[ParseNode] = scala.collection.mutable.Stack[ParseNode]()
              (1 to rule.dropCount) foreach (_ => temp.push(nodeStack.pop()))

              stateStack = stateStack.drop(rule.dropCount)

              nodeStack.push(ParseNode(rule.nonTerminal, temp.toList))
              stateStack.push(transitionTable(stateStack.top)(rule.nonTerminal) match { case ShiftTransition(x) => x })
            }
            case Some(ShiftTransition(_)) => reducing = false
            case None => reducing = false
          }
        }

        nodeStack.push(ParseNode(a))
        //  reject if Trans[stateStack.top; a] = ERROR
        stateStack.push(transitionTable(stateStack.top)(a) match { case ShiftTransition(x) => x })
      }
    } 

    ParseNode("S", nodeStack.toList.reverse)
  }
}
