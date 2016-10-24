import org.scalatest.FunSuite

import dwit._
import Syntax._
import Evaluation._

class TestSuite extends FunSuite {

  def _add: String = "add"

  test("tuple") {
    val p = ETuple(EVal(VNum(1)), EVal(VNum(2)))
    assert(eval(p) == VTuple(VNum(1), VNum(2)))
  }

  test("tree -- leaf") {
    val p = ELeaf
    eval(p) match {
      case Leaf(_) => assert(true)
      case p => assert(false, s"leaf did not equal $p")
    }
  }

  test("tree -- node") {
    val p = ENode(EVal(VNum(1)), ELeaf, ELeaf)

    FreshGen.reset()
    val (res, _, ts) = cekLoop((Left(p), Map(), List()), Map(), Map())

    res match {
      case Node(t, VNum(1), Leaf(t1), Leaf(t2)) => {
        val typeT1 = ts(t1.asInstanceOf[TAlpha]) == t
        val typeT2 = ts(t2.asInstanceOf[TAlpha]) == t
        assert(typeT2 && typeT1, s"Types in the tree don't match: $t, $t1, $t2")
      }
      case p => assert(false, s"leaf did not equal $p")
    }
  }

  test("1 + 1") {
    val p = EAdd(_add, EVal(VNum(1)), EVal(VNum(2)))
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

  test("function application") {
    val p = EApp(EVal(VLambda("x", EAdd(_add, EVar("x"), EVal(VNum(1))), Map())), EVal(VNum(2)))

    FreshGen.reset()
    val (res, _, _) = cekLoop((Left(p), Map(), List()), Map(), Map())
    assert(res == VNum(3))
  }

  test("case of -- product") {
    val tup = ETuple(EVal(VNum(1)), EVal(VNum(2)))
    val p = ECaseOfProduct(tup, List("x", "y"), EAdd(_add, EVar("x"), EVar("y")))
    assert(eval(p) == VNum(3))
  }

  test("case of -- tree: Leaf") {
    val p =
      ECaseOfTree(ELeaf, EVal(VBool(true)), List("x", "y", "z"),
        EVal(VBool(false)))
    assert(eval(p) == VBool(true))
  }

  test("case of -- tree: Node") {
    val p =
      ECaseOfTree(ENode(EVal(VNum(1)), ELeaf, ELeaf), EVal(VBool(true)),
        List("x", "y", "z"), EVar("x"))
    assert(eval(p) == VNum(1))
  }
}
