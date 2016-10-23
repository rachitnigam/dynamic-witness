class WitnessTests extends org.scalatest.FunSuite {
  import dwit._
  import Syntax._
  import Evaluation._
  import Parser._

  test("simple witness") {
    val p = parse("""
      fun x -> if x then x else x + 2
      """
    )
    println(findWitness(p))
  }

}
