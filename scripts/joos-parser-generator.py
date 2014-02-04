import sys
tokenfile = open(sys.argv[2]).read().split("\n")


def token_exists(a):
    search = a + " "
    for x in tokenfile:
        if x.startswith(search):
            return True
    else:
        return False

c_args = (
    "override val children: List[ParseNode] = List.empty[ParseNode], "
    "override val value: Option[String] = None"
)
a_args = (
    "children: List[ParseNode] = List.empty[ParseNode], "
    "value: Option[String] = None"
)

type_template = """
  object %s extends ParseNodeType {
    override def apply(%s): ParseNode =
      new ParseNodes.%s(children, value)
    override def tokenType: Option[TokenType] = %s
  }"""

str_match_template = """    case "%s" => %s"""
match_template = """    case %s => %s"""
caseclass_template = """  case class %s(%s) extends ParseNode {
    def tokenType: Option[TokenType] = %s
  }"""

def consumerule(f):
    line = f.readline().strip()
    tokens = line.split(" ")
    return (tokens[0], tokens[1:])

with open(sys.argv[1]) as f:
    terminals = [f.readline().strip() for _ in xrange(0, int(f.readline()))]
    nonterminals = [f.readline().strip() for _ in xrange(0, int(f.readline()))]
    start = f.readline().strip()
    rules = [consumerule(f) for _ in xrange(0, int(f.readline()))]


print "package joosbox.parser"
print "import joosbox.lexer.TokenType"
print "import joosbox.lexer.TokenTypes"
print
print "object ParseNodeTypes {"
for token in terminals + nonterminals:
    if token_exists(token):
        token_type = "Some(TokenTypes.%s)" % token
    else:
        token_type = "None"
    print type_template % (token, a_args, token, token_type)

print "  def fromString(input: String): ParseNodeType = input match {"
for token in terminals + nonterminals:
    print str_match_template % (token, token)
print "  }"
print
print "  def fromTokenType(input: TokenType) : ParseNodeType = input match {"
for token in terminals + nonterminals:
    if token_exists(token):
        token_type = "TokenTypes.%s" % token
        print match_template % (token_type, token)

print "  }"
print "}"
print
print "object ParseNodes {"
print "  def fromToken(input: Token) : ParseNode = ParseNodeTypes.fromTokenType(input.tokenType)(List.empty[ParseNode], if (input.data == "") None else Some(input.data))"
for token in terminals + nonterminals:
    if token_exists(token):
        token_type = "Some(TokenTypes.%s)" % token
    else:
        token_type = "None"
    print caseclass_template % (token, c_args, token_type)

print "}"
