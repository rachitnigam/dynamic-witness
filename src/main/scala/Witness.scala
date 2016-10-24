package dwit

object Witness {
  import Evaluation._
  import FreshGen._
  import Syntax._

  def findWitness(e: Expr) = _findWitness(0, e)

  // Helper function for witness finding
  private def _findWitness(bound: Int, e: Expr): Option[(ValSubst, TypeSubst)] = try {
    saturateExpr(e)
    // Search for a witness with the bound.
    if (bound >= 100) None
    else _findWitness(bound + 1, e)
  } catch {
    case Stuck(_, vSub, tSub) => {
      // Since we know that the execution got stuck, it will get stuck again in this eval.
      // This means that returning None doesn't actually do anything other than resolving
      // the type error since the code will always hit the Stuck case.
      try {
        saturateWithValsExpr(e, vSub)
        None
      } catch {
        case Stuck(_, _, _) => Some((vSub, tSub))
      }

    }
  }

  // Saturate a function without applications till it does not eval to a lambda.
  def saturateExpr(e: Expr): Value = saturate(eval(e))
  def saturate(v: Value): Value = v match {
    case f@VLambda(_, _, _) => {
      saturate(eval(EApp(EVal(f), EVal(FreshGen.freshHole(FreshGen.freshType)))))
    }
    case _ => v
  }

  // To be used when the trace is being generated.
  def saturateWithValsExpr(e: Expr, vSub: ValSubst): Value = saturateWithVals(eval(e, 1), vSub)
  def saturateWithVals(v: Value, vSub: ValSubst): Value = v match {
    case f: VLambda => {
      val hole = FreshGen.freshHole(FreshGen.freshType)
      val v = vSub(hole)
      saturate(eval(EApp(EVal(f), EVal(v)), 1))
    }
    case _ => v
  }
}
