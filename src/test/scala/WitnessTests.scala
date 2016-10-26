class WitnessTests extends org.scalatest.FunSuite {
  import dwit.Witness._
  import dwit.Parser._

  test("simple witness") {
    val p = parse("""
      fun x -> if x then x else x + 2
      """
    )
    println(findWitness(p))
  }

  test("recursive function") {
    val p = parse("""
      let fac = fix fc ->
        fun x -> if x > 0 then x * fc (x - 1) else true
        in fac
      """
    )
    println(findWitness(p))
  }

  test("tuple function") {
    val p = parse("""
      let tupler tup =
        let f = fst tup in
        if f then f + (snd tup)
        else (snd tup)
      in tupler
      """
    )
    println(findWitness(p))
  }

  test("function generation") {
    val p = parse("""
      fun f ->
        if (f 0) then (f true) + 2
        else 2
    """
    )
    println(findWitness(p))
  }

  test("list generation") {
    val p = parse("""
      let rec sum lst = match lst with
        | [] -> []
        | hd :: tl -> hd + (sum tl)
      in sum"""
      )
    println(findWitness(p))
  }

  ignore("with a helper function") {
    val p = parse("""
      let rec append x xs = match xs with
        | [] -> x :: []
        | hd :: tl -> x :: xs

      in let rec digitsOfInt n =
        if n > 0 then []
        else append (digitsOfInt (n / 10)) (n % 10) :: [] in

      digitsOfInt
        """)
    println(findWitness(p))
  }
}
