package dwit

object Substitution {
  import Syntax._

  type TypeSubst = Map[TAlpha, Type]
  type ValSubst = Map[VHole, Value]

  def apply(typ: Type)(implicit subst: TypeSubst): Type = typ match {
    case TInt | TBool | TFun => typ
    case TTuple(t1, t2) => TTuple(apply(t1), apply(t2))
    case TTree(t) => TTree(apply(t))
    case sub@TAlpha(i) => subst.getOrElse(sub, sub)
  }

  def apply(v: Value)(implicit subst: ValSubst): Value = v match {
    case VNum(_) | VBool(_) | VLambda(_, _, _) | VNil(_) => v
    case hole@VHole(_, _) => subst.getOrElse(hole, hole)
    case VTuple(t1, t2) => VTuple(apply(t1), apply(t2))
    case VCons(t, t1, t2) => {
      VCons(t, apply(t1), apply(t2))
    }
  }

  def compose(alpha: TAlpha, typ: Type, subst: TypeSubst): TypeSubst = {
    // println(s"Composing ($alpha -> $typ) and $subst")
    val normalized = Map(alpha -> apply(typ)(subst))
    subst.mapValues(t => apply(t)(normalized)) ++ normalized
  }

  def compose(hole: VHole, v: Value, subst: ValSubst): ValSubst = {
    val normalized = Map(hole -> apply(v)(subst))
    subst.mapValues(v => apply(v)(normalized)) ++ normalized
  }
}
