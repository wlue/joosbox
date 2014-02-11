import sys

tokens = []
keywords = {}
with open(sys.argv[1]) as f:
    for line in f:
        if line.startswith("//") or line.startswith("#"):
            continue
        parts = [x for x in line.strip().split(" ") if x]
        if not len(parts):
            continue
        name, kind = parts[:2]
        default = None
        if len(parts) == 3:
            default = parts[2]
        if ((name.endswith("Keyword") or name.endswith("Literal"))
                and kind == "Fixed" and default):
            keywords[
                name.replace("Keyword", "").replace("Literal", "").lower()
            ] = name
        tokens.append((name, kind, default))

print """package joosbox.lexer
// WARNING: THIS IS AUTO-GENERATED CODE
// Don't update this file directly, unless you know
// for some reason that the joos-lexer-generator.py script
// will not be re-run.
"""

fixed_template = """
  object %s extends FixedTokenType {
    override def value = "%s"
    override def create(data: InputString): Token = new Tokens.%s(verify(data))
  }"""

variable_template = """
  object %s extends VariableTokenType {
    override def create(data: InputString): Token = new Tokens.%s(verify(data))
  }"""

keyword_template = """
  object %s extends KeywordTokenType {
    override def value = "%s"
    override def create(data: InputString): Token = new Tokens.%s(verify(data))
  }"""

case_class_template = \
    """  case class %s(override val data: InputString) extends %s(data) {
    override def tokenType: TokenType = TokenTypes.%s
  }
"""
match_template = """    case "%s" => %s"""


def is_keyword(string):
    for c in string:
        if ord(c) > ord("z") or ord(c) < ord("a"):
            return False
    else:
        return True

print "object TokenTypes {"

for name, kind, default in tokens:
    if kind == "Fixed":
        if default:
            if is_keyword(default):
                print keyword_template % (name, default, name)
            else:
                print fixed_template % (name, default, name)
        else:
            print fixed_template % (name, "", name)
    elif kind == "Variable":
        print variable_template % (name, name)
print
print "  val Keywords = Map("
kmap = [('    "' + name + '" -> TokenTypes.' + string)
        for name, string in keywords.items()]
print ", \n".join(kmap),
print "\n  )"

print "  def fromString(input: String): TokenType = input match {"
for token, kind, default in tokens:
    print match_template % (token, token)
print "  }"

print "}"
print 
print """

object Tokens {
  //  Classes of token objects themselves - these are instantiated.
"""

for name, kind, default in tokens:
    if kind == "Fixed":
        if default:
            if is_keyword(default):
                print case_class_template % (name, "KeywordToken", name)
            else:
                print case_class_template % (name, "FixedToken", name)
        else:
            print case_class_template % (name, "FixedToken", name)
    elif kind == "Variable":
        print case_class_template % (name, "VariableToken", name)

print "}"
