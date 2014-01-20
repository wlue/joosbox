package joosbox.lexer

//  Note that an InputSymbol is either a valid Unicode
//  character or the empty string, to indicate an epsilon-transition.
case class InputSymbol(symbol: String)