class EvalTests extends org.scalatest.FunSuite {

  import dwit._
  import Parser._
  import Evaluation._

  def parseAndEval(str: String, v: Value) = {
    assert(eval(parse(str)) == v)
  }

  test("increment function") {
    parseAndEval("""(fun x -> x + 1) 1""", VNum(2))
  }
}
