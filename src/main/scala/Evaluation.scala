package dwit
import Syntax._
import GlobalConfig._

object Context {

  sealed trait Kont
  case class KAppL(e: Expr, env: Env) extends Kont
  case class KAppR(v: Value, env: Env) extends Kont

  case class KAddL(e: Expr, op: String, env: Env) extends Kont
  case class KAddR(v: Value, op: String) extends Kont

  case class KIf(c: Expr, a: Expr, env: Env) extends Kont

  case class KProdL(e: Expr, env: Env) extends Kont
  case class KProdR(v: Value) extends Kont

  case class KConsL(next: Expr, env: Env) extends Kont
  case class KConsR(v: Value) extends Kont

  case class KCaseOfProduct(bind: List[Id], e: Expr, env: Env) extends Kont
  case class KCaseOfTree(leafBody: Expr, bind: List[Id], nodeBody: Expr, env: Env) extends Kont
}

object Evaluation {
  import Context._
  import Helpers._
  import Substitution._

  type EoV = Either[Expr, Value]
  type Stack = List[Kont]
  type State = (EoV, Env, Stack)

  case class Stuck(msg: String, vSub: ValSubst, tSub: TypeSubst) extends Exception(
    s"Stuck errors should be caught: $msg\ntypeSub: $tSub\nvalueSub: $vSub"
  )

  case object Unreachable extends Exception("Hit a case that should be impossible")

  private def eitherToString(e: Either[Any, Any]) = e match {
    case Left(e) => e.toString
    case Right(e) => e.toString
  }

  def eval(e: Expr): Value = {
    cekLoop((Left(e), Map(), List()), Map(), Map())._1
  }

  def cekLoop(state: State, vSub: ValSubst, tSub: TypeSubst):
    (Value, ValSubst, TypeSubst) = {
    state match {
      case (Right(v), _, Nil) => (v, vSub, tSub)
      case state => {
        log1(s"${eitherToString(state._1)}")
        log2(s"$state, $vSub, $tSub\n")
        val (s, vs, ts) = cek(state, vSub, tSub)
        cekLoop(s, vs, ts)
      }
    }
  }

  def substFix(fb: Expr, fName: Id)(implicit fixExpr: Expr): Expr = fb match {
    case vfun@EVal(VLambda(i, b, e)) => {
      if (i == fName) vfun else EVal(VLambda(i, substFix(fb, fName), e))
    }
    case EVal(_) | ENil => fb
    case EVar(i) => if (i == fName) fixExpr else fb
    case EFun(i, b) => if (i == fName) fb else EFun(i, substFix(b, fName))
    case EFix(fn, fb) => if (fn == fName) fb else EFix(fn, substFix(fb, fName))
    case EApp(e1, e2) => EApp(substFix(e1, fName), substFix(e2, fName))
    case EBinOp(op, e1, e2) => EBinOp(op, substFix(e1, fName), substFix(e2, fName))
    case EITE(e1, e2, e3) => EITE(substFix(e1, fName), substFix(e2, fName), substFix(e3, fName))
    case ETuple(e1, e2) => ETuple(substFix(e1, fName), substFix(e2, fName))
    case ECons(e1, e2) => ECons(substFix(e1, fName), substFix(e2, fName))
    case ECaseOfProduct(e, b, body) => ECaseOfProduct(substFix(e, fName), b, substFix(body, fName))
    case ECaseOfTree(e, lb, binds, nb) => {
      ECaseOfTree(substFix(e, fName), substFix(lb, fName), binds, substFix(nb, fName))
    }
  }

  def toValue(v: Any): Value = v match {
    case _ : Boolean => VBool(v.asInstanceOf[Boolean])
    case _ : Int => VNum(v.asInstanceOf[Int])
    case _ => error(Unreachable)
  }

  def applyFun(op: String, v1: Int, v2: Int): Value = toValue(op match {
    case "add" => v1 + v2
    case "sub" => v1 - v2
    case "mul" => v1 * v2
    case "div" => v1 / v2
    case "mod" => v1 % v2
    case "eq" => v1 == v2
    case "gt" => v1 > v2
    case "lt" => v1 < v2
  })

  def cek(state: State, vSub: ValSubst, tSub: TypeSubst): (State, ValSubst, TypeSubst) = {
    state match {
    case (Left(e), env, kont) => e match {
      case EVal(v) => ((Right(v), env, kont), vSub, tSub)
      case EVar(i) => ((Right(env(i)), env, kont), vSub, tSub)
      case EFun(i, b) => ((Right(VLambda(i, b, env)), env, kont), vSub, tSub)
      case fixExpr@EFix(fn, fb) => {
        val sfb = substFix(fb, fn)(fixExpr)
        ((Left(sfb), env, kont), vSub, tSub)
      }
      case EApp(e1, e2) => ((Left(e1), env, KAppL(e2, env) :: kont), vSub, tSub)
      case EBinOp(op, e1, e2) => ((Left(e1), env, KAddL(e2, op, env) :: kont), vSub, tSub)
      case EITE(p, c, a) => ((Left(p), env, KIf(c, a, env) :: kont), vSub, tSub)
      case ETuple(e1, e2) => ((Left(e1), env, KProdL(e2, env) :: kont), vSub, tSub)
      case ECaseOfProduct(e, b, body) => {
        ((Left(e), env, KCaseOfProduct(b, body, env) :: kont), vSub, tSub)
      }
      case ECaseOfTree(e, lb, b, nb) => {
       ((Left(e), env, KCaseOfTree(lb, b, nb, env) :: kont), vSub, tSub)
      }
      case ECons(e1, e2) => ((Left(e1), env, KConsL(e2, env) :: kont), vSub, tSub)
      case ENil => ((Right(VNil(FreshGen.freshType())), env, kont), vSub, tSub)
    }

    case (Right(_), _, Nil) => (state, vSub, tSub)

    case (Right(v), topEnv, kTop::kont) => kTop match {
      case KAppL(e, env) => ((Left(e), env, KAppR(v, env) :: kont), vSub, tSub)

      case KAppR(fun, _) => narrow(fun, TFun, vSub, tSub) match {
        case (None, v, t) => error(Stuck(s"Failed on $kTop and $v", v, t))
        case (Some(VLambda(i, e, fEnv)), vs, ts) => ((Left(e), fEnv + (i -> v), kont), vs, ts)
        case (Some(_), _, _) => error(Unreachable)
      }

      case KAddL(e, op, env) => ((Left(e), env, KAddR(v, op) :: kont), vSub, tSub)

      case KAddR(r, op) => narrow(v, TInt, vSub, tSub) match {
        case (None, v, t) => error(Stuck(s"Failed on $kTop and $v", v, t))
        case (Some(VNum(lv)), vs, ts) => {
          narrow(r, TInt, vs, ts) match {
            case (None, vs2, ts2) => error(Stuck(s"Failed on $kTop and $v", vs2, ts2))
            case (Some(VNum(rv)), vs2, ts2) => {
              ((Right(applyFun(op, rv, lv)), topEnv, kont), vs2, ts)
            }
            case _ => error(Unreachable)
          }
        }
        case (Some(_), _, _) => error(Unreachable)
      }

      case KIf(c, a, env) => narrow(v, TBool, vSub, tSub) match {
        case (None, vs, ts) => error(Stuck(s"Failed on $kTop and $v", vs, ts))
        case (Some(VBool(true)), vs, ts) => ((Left(c), env, kont), vs, ts)
        case (Some(VBool(false)), vs, ts) => ((Left(a), env, kont), vs, ts)
        case _ => error(Unreachable)
      }

      case KProdL(e, env) => ((Left(e), env, KProdR(v) :: kont), vSub, tSub)
      case KProdR(vr) => ((Right(VTuple(vr, v)), topEnv, kont), vSub, tSub)

      case KConsL(e1, env) => ((Left(e1), env, KConsR(v) :: kont), vSub, tSub)
      case KConsR(head) => {
        val t = typeOf(head)
        narrow(v, TList(t), vSub, tSub) match {
          case (None, vs, ts) => error(Stuck(s"Failed on $kTop and $v", vs, ts))
          case (Some(tail), vs, ts) => {
                ((Right(VCons(t, head, tail)), topEnv, kont), vs, ts)
          }
        }
      }

      case KCaseOfProduct(binds, e, env) => {
        val a1 = FreshGen.freshType()
        val a2 = FreshGen.freshType()
        narrow(v, TTuple(a1, a2), vSub, tSub) match {
          case (None, vs, ts) => error(Stuck(s"Failed on $kTop and $v", vs, ts))
          case (Some(VTuple(v1, v2)), vs, ts) => {
            ((Left(e), env ++ binds.zip(List(v1, v2)).toMap, kont), vs, ts)
          }
          case _ => error(Unreachable)
        }
      }

      case KCaseOfTree(lb, binds, nb, env) => {
        narrow(v, TList(FreshGen.freshType()), vSub, tSub) match {
          case (None, vs, ts) => error(Stuck(s"Failed on $kTop and $v", vs, ts))
          case (Some(VNil(t)), vs, ts) => ((Left(lb), env, kont), vs, ts)
          case (Some(VCons(t, v1, v2)), vs, ts) => {
            val bindMap = binds.zip(List(v1, v2)).toMap
            ((Left(nb), env ++ bindMap, kont), vs, ts)
          }
          case _ => error(Unreachable)
        }
      }
    }
  }}
}
