package joosbox.parser 

import scala.util.control.Exception._ 
import joosbox.lexer.TokenTypes
import joosbox.lexer.TokenType
import joosbox.lexer.Tokens
import joosbox.lexer.Token
import joosbox.lexer.TokenNFA

class SyntaxError(msg: String) extends RuntimeException(msg)

class ProductionRule(val nonTerminal: ParseNodeType, val others: List[ParseNodeType]) {
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
  lazy val Joos: Parser = Parser.fromLR1File("joos1w.lr1")

  def fromLR1Definition(definition: String): Parser = {
    val lines:Array[String] = definition.split("\n")

    val numTerminalSymbols:Int = lines(0).toInt
    val terminalSymbols:Set[ParseNodeType] = lines.slice(1, 1 + numTerminalSymbols).toSet.map((s: String) => ParseNodeTypes.fromString(s))

    val numNonTerminalSymbols:Int = lines(numTerminalSymbols + 1).toInt
    val nonTerminalSymbols:Set[ParseNodeType] =
      lines.slice(numTerminalSymbols + 2,
                  numTerminalSymbols + 2 + numNonTerminalSymbols).toSet.map((s: String) => ParseNodeTypes.fromString(s))

    if (nonTerminalSymbols.intersect(terminalSymbols).size > 0) {
      throw new IllegalArgumentException("Symbols shared between nonterminals and terminals!")
    }

    val startSymbol:ParseNodeType = ParseNodeTypes.fromString(lines(numTerminalSymbols + numNonTerminalSymbols + 2))
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
        val nonTerminal: ParseNodeType = ParseNodeTypes.fromString(tokens.head)
        if (!nonTerminalSymbols.contains(nonTerminal)) {
          throw new IllegalArgumentException("Nonterminal on left side of production rule is not in nonterminal symbol set.")
        }

        val otherTokens: Array[ParseNodeType] = tokens.drop(1).map((s: String) => ParseNodeTypes.fromString(s))
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

    val transitionTable:Map[Int, Map[ParseNodeType, Transition]] = transitionStrings.foldLeft(Map.empty[Int, Map[ParseNodeType, Transition]]) {
      case (map: Map[Int, Map[ParseNodeType, Transition]], transitionString: String) => {
        val segments:Array[String] = transitionString.split(" ")
        if (segments.size == 4) {
          val (sourceStateString, symbol_s, action, targetString) = (segments(0), segments(1), segments(2), segments(3))
          val symbol:ParseNodeType = ParseNodeTypes.fromString(symbol_s)
            
          val sourceState:Int = sourceStateString.toInt
          val target:Int = targetString.toInt
          val transition:Transition = action match {
            case "shift" => new ShiftTransition(target)
            case "reduce" => new ReduceTransition(productionRules(target))
          }

          map + (sourceState -> (map.getOrElse(sourceState, Map.empty[ParseNodeType, Transition]) + (symbol -> transition)))
        } else {
          map
        }
      }
    }

    new Parser(terminalSymbols, nonTerminalSymbols, startSymbol, productionRules, transitionTable)
  }

  def fromLR1File(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    val definition = source.mkString
    source.close()
    fromLR1Definition(definition)
  }
}

class Parser(
  val terminalSymbols: Set[ParseNodeType],
  val nonTerminalSymbols: Set[ParseNodeType],
  val startSymbol: ParseNodeType,
  val productionRules: List[ProductionRule],
  val transitionTable: Map[Int, Map[ParseNodeType, Transition]]
) {
  lazy val lexer = TokenNFA.nfa.toDFA

  def parseFilename(filename: String): ParseNode = parseString(scala.io.Source.fromFile(filename).mkString)
  def parseString(str: String): ParseNode = parse(lexer.matchString(str).get)

  def parse(_symbols: List[joosbox.lexer.Token]): ParseNode = {
    //  Remove whitespace. TODO: Should we be doing this here?
    val symbols: List[joosbox.lexer.Token] =
      _symbols.filter((s: joosbox.lexer.Token) => s.tokenType != TokenTypes.Whitespace)

    var nodeStack : scala.collection.mutable.Stack[ParseNode] = scala.collection.mutable.Stack[ParseNode]()
    var stateStack : scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int]()

    nodeStack.push(ParseNodes.BOF())
    stateStack.push(transitionTable(0)(ParseNodeTypes.BOF) match { case ShiftTransition(x) => x })

    (symbols ++ List[joosbox.lexer.Token](joosbox.lexer.TokenTypes.EOF())).foreach {
      (a: Token) => {
        var reducing = true
        while (reducing) {
          transitionTable(stateStack.top).get(ParseNodeTypes.fromTokenType(a.tokenType)) match {
            case Some(ReduceTransition(rule: ProductionRule)) => {
              val temp : scala.collection.mutable.Stack[ParseNode] = scala.collection.mutable.Stack[ParseNode]()
              (1 to rule.dropCount) foreach (_ => temp.push(nodeStack.pop()))

              stateStack = stateStack.drop(rule.dropCount)

              nodeStack.push(rule.nonTerminal(temp.toList))
              stateStack.push(transitionTable(stateStack.top)(rule.nonTerminal) match {
                case ShiftTransition(x) => x
              })
            }
            case Some(ShiftTransition(_)) => reducing = false
            case None => reducing = false
          }
        }

        //  Create a new parse node from the token
        nodeStack.push(ParseNodes.fromToken(a))
        
        //  reject if Trans[stateStack.top; a] = ERROR
        val newPossibleStates: Map[ParseNodeType, Transition] = transitionTable(stateStack.top)
        newPossibleStates.get(ParseNodeTypes.fromTokenType(a.tokenType)) match {
          case Some(ShiftTransition(newState)) => stateStack.push(newState)
          case None => {
            throw new SyntaxError("Got '" + a + "', expected one of: " + newPossibleStates.keys.map(_.getClass.getSimpleName).mkString(", "))
          }
        }
      }
    } 

    new ParseNodes.S(nodeStack.toList.reverse)
  }
}
