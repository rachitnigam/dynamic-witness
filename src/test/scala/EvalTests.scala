class EvalTests extends org.scalatest.FunSuite {

  import dwit._
  import Parser._
  import Syntax._
  import Evaluation._

  def parseAndEval(str: String, v: Value) = {
    assert(eval(parse(str)) == v)
  }

  test("increment function") {
    parseAndEval("""(fun x -> x + 1) 1""", VNum(2))
  }

  test("add three numbers") {
    parseAndEval("""1 + 2 + 3""", VNum(6))
  }

  test("let statement") {
    parseAndEval("""let x = 1 in x + 1""", VNum(2))
  }
}
