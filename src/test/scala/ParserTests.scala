class ParserTests extends org.scalatest.FunSuite {
  import dwit._
  import Syntax._
  import Parser._

  def testProg(str: String, prog: Expr) = {
    assert(parse(str) == prog)
  }

  test("numbers") {
    testProg("""1""", EVal(VNum(1)))
  }

  test("booleans") {
    testProg("""true""", EVal(VBool(true)))
    testProg("""false""", EVal(VBool(false)))
  }

  test("vars") {
    testProg("""x""", EVar("x"))
    testProg("""_x""", EVar("_x"))
  }

  test("tuple") {
    testProg("""(1, 2)""", ETuple(EVal(VNum(1)), EVal(VNum(2))))
    testProg("""(true, false)""", ETuple(EVal(VBool(true)), EVal(VBool(false))))
  }
}
