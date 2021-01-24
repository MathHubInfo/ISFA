import org.scalatest.FlatSpec

import parser.Num
import sage.SageWrapper

class SageTest extends FlatSpec {

  "sage wrapper" should "simplifiy in order" in {
    val expressions = (1 to 100).map(Num(_))
    expressions.foreach { x =>
      SageWrapper.simplify(x).foreach(y => assert(y == x))
    }
  }
}
