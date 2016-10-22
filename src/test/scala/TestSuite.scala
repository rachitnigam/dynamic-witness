import org.scalatest.FunSuite

import dwit._
import Syntax._
import Evaluation._

class SimpleTests extends FunSuite {

  test("1 + 1") {
    val p = EAdd(EVal(VNum(1)), EVal(VNum(2)))
    assert(eval(p) == VNum(3))
  }

  test("if true") {
    val p = EITE(EVal(VBool(true)), EVal(VNum(1)), EVal(VNum(2)))
    assert(eval(p) == VNum(1))
  }

  test("if false") {
    val p = EITE(EVal(VBool(false)), EVal(VNum(1)), EVal(VNum(2)))
    assert(eval(p) == VNum(2))
  }

}
