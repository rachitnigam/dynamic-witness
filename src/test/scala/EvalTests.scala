class EvalTests extends org.scalatest.FunSuite {

  import dwit._
  import Parser._
  import Syntax._
  import Evaluation._

  def parseAndEval(str: String, v: Value) = {
    assert(eval(parse(str)) == v)
  }

  test("sub numbers (order of operation)") {
    parseAndEval("""2 - 1""", VNum(1))
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

  test("higher order function") {
    parseAndEval(
      """let f = (fun x -> fun y -> x + y) in
            let inc = f 10 in
              inc 2""",
    VNum(12))
  }

  test("if expressions -- true") {
    parseAndEval("""
      if true then 1 else 2
      """, VNum(1)
    )
  }

  test("if expressions -- false") {
    parseAndEval("""
      if false then 1 else 2
      """, VNum(2)
    )
  }

  test("boolean connective -- and") {
    parseAndEval("""
      true and false
      """, VBool(false)
    )
  }

  test("boolean connective -- or") {
    parseAndEval("""
      true or false
      """, VBool(true)
    )
  }

  test("if expression with keywords") {
    parseAndEval("""
      if true and false then 1 else 0
      """, VNum(0))
  }

  test("test comparison -- eq") {
    parseAndEval("""1 = 1""", VBool(true))
  }

  test("test comparison -- lt") {
    parseAndEval("""1 < 2""", VBool(true))
  }

  test("test comparison -- gt") {
    parseAndEval("""2 > 1""", VBool(true))
  }

  test("shadowing works") {
    parseAndEval("""
      let x = 1 in
        let x = x + x in
          x""", VNum(2)
    )
  }

  test("functions use local env") {
    parseAndEval("""
      let y = 10 in
        let f = fun x -> x + y in
          let y = 2 in
            f 20""", VNum(30))
  }
}
