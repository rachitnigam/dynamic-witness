package dwit
import Syntax._

object Context {
  type Env = Map[Id, Value]

  sealed trait Kont
  case class KAppL(e: Expr, env: Env) extends Kont
  case class KAppR(v: Value, env: Env) extends Kont

  case class KAddL(e: Expr, op: (Int, Int) => Int, env: Env) extends Kont
  case class KAddR(v: Value, op: (Int, Int) => Int) extends Kont

  case class KCmpL(e: Expr, op: (Int, Int) => Boolean, env: Env) extends Kont
  case class KCmpR(v: Value, op: (Int, Int) => Boolean) extends Kont

  case class KIf(c: Expr, a: Expr, env: Env) extends Kont

  case class KProdL(e: Expr, env: Env) extends Kont
  case class KProdR(v: Value) extends Kont

  case class KNode1(next: Expr, e: Expr, env: Env) extends Kont
  case class KNode2(v: Value, next: Expr, env: Env) extends Kont
  case class KNode3(v1: Value, v2: Value) extends Kont

  case class KCaseOfProduct(bind: List[Id], e: Expr, env: Env) extends Kont
  case class KCaseOfTree(leafBody: Expr, bind: List[Id], nodeBody: Expr, env: Env) extends Kont
}

object Evaluation {
  import Context._
  import Helpers._

  type EoV = Either[Expr, Value]
  type Stack = List[Kont]
  type State = (EoV, Env, Stack)

  case class Stuck(msg: String, vSub: ValSubst, tSub: TypeSubst) extends Exception(
    s"Stuck errors should be caught: $msg\ntypeSub: $tSub\nvalueSub: $vSub"
  )

  def eval(e: Expr): Value = {
    FreshGen.reset()
    cekLoop((Left(e), Map(), List()), Map(), Map())._1
  }

  def saturate(v: Value): Value = v match {
    case f@VLambda(_, _, _) => {
      saturate(eval(EApp(EVal(f), EVal(FreshGen.freshHole(FreshGen.freshType)))))
    }
    case _ => v
  }

  def cekLoop(state: State, vSub: ValSubst, tSub: TypeSubst):
    (Value, ValSubst, TypeSubst) = {
    state match {
      case (Right(v), _, Nil) => (v, vSub, tSub)
      case state => {
        println(s"$state, $vSub, $tSub")
        val (s, vs, ts) = cek(state, vSub, tSub)
        cekLoop(s, vs, ts)
      }
    }
  }

  def cek(state: State, vSub: ValSubst, tSub: TypeSubst): (State, ValSubst, TypeSubst) = {
    state match {
    case (Left(e), env, kont) => e match {
      case EVal(v) => ((Right(v), env, kont), vSub, tSub)
      case EVar(i) => ((Right(env(i)), env, kont), vSub, tSub)
      case EFun(i, b) => ((Right(VLambda(i, b, env)), env, kont), vSub, tSub)
      case EApp(e1, e2) => ((Left(e1), env, KAppL(e2, env) :: kont), vSub, tSub)
      case EAdd(op, e1, e2) => ((Left(e1), env, KAddL(e2, op, env) :: kont), vSub, tSub)
      case ECmp(op, e1, e2) => ((Left(e1), env, KCmpL(e2, op, env) :: kont), vSub, tSub)
      case EITE(p, c, a) => ((Left(p), env, KIf(c, a, env) :: kont), vSub, tSub)
      case ETuple(e1, e2) => ((Left(e1), env, KProdL(e2, env) :: kont), vSub, tSub)
      case ECaseOfProduct(e, b, body) => {
        ((Left(e), env, KCaseOfProduct(b, body, env) :: kont), vSub, tSub)
      }
      case ECaseOfTree(e, lb, b, nb) => {
       ((Left(e), env, KCaseOfTree(lb, b, nb, env) :: kont), vSub, tSub)
      }
      case ENode(e1, e2, e3) => ((Left(e1), env, KNode1(e2, e3, env) :: kont), vSub, tSub)
      case ELeaf => ((Right(Leaf(FreshGen.freshType())), env, kont), vSub, tSub)
    }

    case (Right(_), _, Nil) => (state, vSub, tSub)

    case (Right(v), topEnv, kTop::kont) => kTop match {
      case KAppL(e, env) => ((Left(e), env, KAppR(v, env) :: kont), vSub, tSub)

      case KAppR(fun, _) => narrow(fun, TFun, vSub, tSub) match {
        case (None, v, t) => throw Stuck(s"Failed on $kTop and $v", v, t)
        case (Some(VLambda(i, e, fEnv)), vs, ts) => ((Left(e), fEnv + (i -> v), kont), vs, ts)
        case (Some(_), _, _) => ??? //Unreachable code
      }

      case KAddL(e, op, env) => ((Left(e), env, KAddR(v, op) :: kont), vSub, tSub)

      case KAddR(r, op) => narrow(v, TInt, vSub, tSub) match {
        case (None, v, t) => throw Stuck(s"Failed on $kTop and $v", v, t)
        case (Some(VNum(lv)), vs, ts) => {
          narrow(r, TInt, vs, ts) match {
            case (None, vs2, ts2) => throw Stuck(s"Failed on $kTop and $v", vs2, ts2)
            case (Some(VNum(rv)), vs2, ts2) => ((Right(VNum(op(rv, lv))), topEnv, kont), vs2, ts)
            case _ => ??? // Unreachable Code
          }
        }
        case (Some(_), _, _) => ??? //Unreachable code
      }

      case KCmpL(e, op, env) => ((Left(e), env, KCmpR(v, op) :: kont), vSub, tSub)

      case KCmpR(r, op) => narrow(v, TInt, vSub, tSub) match {
        case (None, v, t) => throw Stuck(s"Failed on $kTop and $v", v, t)
        case (Some(VNum(lv)), vs, ts) => {
          narrow(r, TInt, vs, ts) match {
            case (None, vs2, ts2) => throw Stuck(s"Failed on $kTop and $v", vs2, ts2)
            case (Some(VNum(rv)), vs2, ts2) => ((Right(VBool(op(rv, lv))), topEnv, kont), vs2, ts)
            case _ => ??? // Unreachable Code
          }
        }
        case (Some(_), _, _) => ??? //Unreachable code
      }

      case KIf(c, a, env) => narrow(v, TBool, vSub, tSub) match {
        case (None, vs, ts) => throw Stuck(s"Failed on $kTop and $v", vs, ts)
        case (Some(VBool(true)), vs, ts) => ((Left(c), env, kont), vs, ts)
        case (Some(VBool(false)), vs, ts) => ((Left(a), env, kont), vs, ts)
        case _ => ??? // Unreachable code
      }

      case KProdL(e, env) => ((Left(e), env, KProdR(v) :: kont), vSub, tSub)
      case KProdR(vr) => ((Right(VTuple(vr, v)), topEnv, kont), vSub, tSub)
      case KNode1(e1, e2, env) => ((Left(e1), env, KNode2(v, e2, env) :: kont), vSub, tSub)
      case KNode2(v1, next, env) => ((Left(next), env, KNode3(v1, v) :: kont), vSub, tSub)
      case KNode3(v1, v2) => {
        val t = typeOf(v1)
        narrow(v2, TTree(t), vSub, tSub) match {
          case (None, vs, ts) => throw Stuck(s"Failed on $kTop and $v", vs, ts)
          case (Some(v2_res), vs, ts) => {
            narrow(v, TTree(t), vs, ts) match {
              case (None, vs2, ts2) => throw Stuck(s"Failed on $kTop and $v", vs, ts)
              case (Some(v3_res), vs2, ts2) => {
                ((Right(Node(t, v1, v2_res, v3_res)), topEnv, kont), vs2, ts2)
              }
            }
          }
        }
      }

      case KCaseOfProduct(binds, e, env) => {
        val a1 = FreshGen.freshType()
        val a2 = FreshGen.freshType()
        narrow(v, TTuple(a1, a2), vSub, tSub) match {
          case (None, vs, ts) => throw Stuck(s"Failed on $kTop and $v", vs, ts)
          case (Some(VTuple(v1, v2)), vs, ts) => {
            ((Left(e), binds.zip(List(v1, v2)).toMap ++ env, kont), vs, ts)
          }
          case _ => ??? // Unreachable code
        }
      }

      case KCaseOfTree(lb, binds, nb, env) => {
        narrow(v, TTree(FreshGen.freshType()), vSub, tSub) match {
          case (None, vs, ts) => throw Stuck(s"Failed on $kTop and $v", vs, ts)
          case (Some(Leaf(t)), vs, ts) => ((Left(lb), env, kont), vs, ts)
          case (Some(Node(t, v1, v2, v3)), vs, ts) => {
            val bindMap = binds.zip(List(v1, v2, v3)).toMap
            ((Left(nb), bindMap ++ env, kont), vs, ts)
          }
          case _ => ??? // Unreachable Code
        }
      }
    }
  }}
}
