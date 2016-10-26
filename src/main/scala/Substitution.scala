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

  def normalizeValues(v: Value)(implicit subst: ValSubst): Value = v match {
    case h@VHole(_, _) => subst.getOrElse(h, h)
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

  def apply(v: Value)(implicit subst: ValSubst): Value = normalizeValues(v)(subst)

  def compose(alpha: TAlpha, typ: Type, subst: TypeSubst): TypeSubst = {
    val normalized = Map(alpha -> apply(typ)(subst))
    subst.mapValues(t => apply(t)(normalized)) ++ normalized
  }

  def compose(hole: VHole, v: Value, subst: ValSubst): ValSubst = {
    val normalized = Map(hole -> apply(v)(subst))
    subst.mapValues(v => apply(v)(normalized)) ++ normalized
  }
}
