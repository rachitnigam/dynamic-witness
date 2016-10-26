package dwit
import GlobalConfig._

object Witness {

  import Evaluation._
  import FreshGen._
  import Syntax._
  import Substitution._

  def findWitness(e: Expr, bound: Int = 100): List[Value] = {
    _findWitness(0, e, bound)
  }

  case class WitnessNotFound(e: Expr) extends RuntimeException(s"Could not find witness for $e")

  // Helper function for witness finding
  private def _findWitness(bound: Int, e: Expr, ubound: Int): List[Value] = try {
    FreshGen.reset()
    saturateExpr(e)
    // Search for a witness with the bound.
    if (bound >= ubound) error(WitnessNotFound(e))
    else _findWitness(bound + 1, e, ubound)
  } catch {
    case Stuck(_, vSub, tSub) => {
      println(vSub)
      getArgumentsExpr(e, vSub)
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
  def getArgumentsExpr(e: Expr, vSub: ValSubst): List[Value] = {
    FreshGen.reset()
    getArgumentsVal(eval(e), vSub, List())
  }
  def getArgumentsVal(v: Value, vSub: ValSubst, acc: List[Value]): List[Value] = v match {
    case f: VLambda => {
      val hole = FreshGen.freshHole(FreshGen.freshType)
      val v = vSub(hole)
      try {
        getArgumentsVal(eval(EApp(EVal(f), EVal(v))), vSub, acc :+ v)
      } catch {
        case Stuck(_, _, _) => acc :+ v
      }
    }
    case _ => acc
  }
}
