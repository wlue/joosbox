package joosbox.lexer.test

import org.specs2.mutable._

import joosbox.lexer._

class StateSpec extends Specification {
  "State" should {
    "implicitly convert from string" in {
      import StateImplicits._

      val state: State = "s"
      state.isInstanceOf[State]
    }
  }
}
