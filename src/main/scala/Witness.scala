package dwit

object Witness {

  import Evaluation._
  import FreshGen._
  import Syntax._
  import Substitution._

  def findWitness(e: Expr): (ValSubst, TypeSubst) = _findWitness(0, e)

  case class WitnessNotFound(e: Expr) extends RuntimeException(s"Could not find witness for $e")

  // Helper function for witness finding
  private def _findWitness(bound: Int, e: Expr): (ValSubst, TypeSubst) = try {
    FreshGen.reset()
    saturateExpr(e)
    // Search for a witness with the bound.
    if (bound >= 1000) throw WitnessNotFound(e)
    else _findWitness(bound + 1, e)
  } catch {
    case Stuck(_, vSub, tSub) => {
      // Since we know that the execution got stuck, it will get stuck again in this eval.
      // This means that returning None doesn't actually do anything other than resolving
      // the type error since the code will always hit the Stuck case.
      try {
        saturateWithValsExpr(e, vSub)
        throw WitnessNotFound(e)
      } catch {
        case Stuck(_, _, _) => (vSub, tSub)
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

  def normalizeValues(v: Value)(implicit subst: ValSubst): Value = v match {
    case h@VHole(_, _) => subst(h)
    case VNum(_) | VBool(_) | VNil(_) => v
    case VTuple(t1, t2) => VTuple(normalizeValues(t1), normalizeValues(t2))
    case VCons(t, l1, l2) => VCons(t, normalizeValues(l1), normalizeValues(l2))
    case VLambda(id, b, e) => VLambda(id, normalizeExpr(b), e)
  }

  def normalizeExpr(e: Expr)(implicit subst: ValSubst): Expr = e match {
    case EVal(v) => EVal(normalizeValues(v))
    case EVar(_) | ENil => e
    case EFun(id, b) => EFun(id, normalizeExpr(b))
    case EFix(fName, fb) => EFix(fName, normalizeExpr(fb))
    case EApp(e1, e2) => EApp(normalizeExpr(e1), normalizeExpr(e2))
    case EAdd(op, e1, e2) => EAdd(op, normalizeExpr(e1), normalizeExpr(e2))
    case EITE(e1, e2, e3) => EITE(normalizeExpr(e1), normalizeExpr(e2), normalizeExpr(e3))
    case ETuple(e1, e2) => ETuple(normalizeExpr(e1), normalizeExpr(e2))
    case ECons(e1, e2) => ECons(normalizeExpr(e1), normalizeExpr(e2))
    case ECaseOfProduct(e, bs, b) => ECaseOfProduct(normalizeExpr(e), bs, normalizeExpr(b))
    case ECaseOfTree(e, lb, bs, b) => {
      ECaseOfTree(normalizeExpr(e), normalizeExpr(lb), bs, normalizeExpr(b))
    }
  }

  // To be used when the trace is being generated.
  def saturateWithValsExpr(e: Expr, vSub: ValSubst): Value = {
    FreshGen.reset()
    saturateWithVals(eval(e, 0), vSub)
  }
  def saturateWithVals(v: Value, vSub: ValSubst): Value = v match {
    case f: VLambda => {
      val hole = FreshGen.freshHole(FreshGen.freshType)
      val v = vSub(hole)
      saturate(eval(EApp(EVal(f), EVal(v)), 0))
    }
    case _ => v
  }
}
