package dwit
import Syntax._

object Substitution {
  def apply(typ: Type)(implicit subst: TypeSubst): Type = typ match {
    case TInt | TBool | TFun => typ
    case TTuple(t1, t2) => TTuple(apply(t1), apply(t2))
    case TTree(t) => TTree(apply(t))
    case sub@TAlpha(i) => subst.getOrElse(sub, sub)
  }

  def apply(v: Value)(implicit subst: ValSubst): Value = v match {
    case VNum(_) | VBool(_) | VLambda(_, _) | Leaf(_) => v
    case hole@VHole(_, _) => subst.getOrElse(hole, hole)
    case VTuple(t1, t2) => VTuple(apply(t1), apply(t2))
    case Node(t, t1, t2, t3) => {
      Node(t, apply(t1), apply(t2), apply(t3))
    }
  }

  def compose(alpha: TAlpha, typ: Type, subst: TypeSubst): TypeSubst = {
    println(s"Composing ($alpha -> $typ) and $subst")
    val normalized = Map(alpha -> apply(typ)(subst))
    normalized ++ subst.mapValues(t => apply(t)(normalized))
  }

  def compose(hole: VHole, v: Value, subst: ValSubst): ValSubst = {
    val normalized = Map(hole -> apply(v)(subst))
    normalized ++ subst.mapValues(v => apply(v)(normalized))
  }
}
