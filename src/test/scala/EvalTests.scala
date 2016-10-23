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

  test("let bindings for functions") {
    parseAndEval("""
      let f x = x + 1 in
        f 10""", VNum(11)
        )
  }

  test("let bindings for multi-arg functions") {
    parseAndEval("""
      let f x y z = x + 2*y + 3*z in
        f 1 2 3""", VNum(14))
  }

  test("functions use local env") {
    parseAndEval("""
      let y = 10 in
        let f = fun x -> x + y in
          let y = 2 in
            f 20""", VNum(30))
  }

  test("recursive function") {
    parseAndEval("""
      let fac = fix fc ->
        fun x ->
          if x = 0 then
            1
          else
            x * fc (x - 1)
      in fac 5""", VNum(120)
      )
  }

  test("scope for fix works") {
    parseAndEval("""
      let fac = fix fac ->
        fun x ->
          if x = 0 then
            1
          else
            x * fac (x - 1)
      in fac 5""", VNum(120)
      )
  }

  test("let rec works") {
    parseAndEval("""
      let rec fac x = if x > 0 then x * fac (x - 1) else 1
      in fac 5""", VNum(120)
      )
  }

  test("recursive function with two arguments works") {
    parseAndEval("""
      let rec add x y = if x = 0 then y else 1 + (add (x - 1) y)
      in add 4 6""", VNum(10)
      )
  }
}
