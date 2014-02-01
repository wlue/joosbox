package joosbox.parser.test

import org.specs2.mutable._
import joosbox.parser._

class ParserSpec extends Specification {
  "Parser" should {
    "consume basic lr1 grammar" in {
      val lr1 = """6
BOF
EOF
id
-
(
)
3
S
expr
term
S
5
S BOF expr EOF
expr term
expr expr - term
term id
term ( expr )
11
29
1 ( shift 1
8 ( shift 1
9 EOF reduce 2
3 ( shift 1
1 expr shift 2
4 EOF reduce 3
10 - shift 3
6 - reduce 1
1 id shift 4
4 - reduce 3
4 ) reduce 3
10 EOF shift 5
1 term shift 6
7 - reduce 4
7 ) reduce 4
8 term shift 6
3 id shift 4
9 - reduce 2
8 id shift 4
5 BOF reduce 0
2 ) shift 7
0 BOF shift 8
9 ) reduce 2
7 EOF reduce 4
2 - shift 3
6 ) reduce 1
6 EOF reduce 1
3 term shift 9
8 expr shift 10
      """
      Parser.fromLR1Definition(lr1)
      true
    }
  }
}
