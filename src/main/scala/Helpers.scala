package dwit
import Syntax._

object FreshGen {

  private var typeId: TypeId = 0
  private var holeId: HoleId = 0

  def freshHole(typ: Type): VHole = {
    holeId += 1;
    VHole(holeId, typ)
  }

  def freshType(): TAlpha = {
    typeId += 1;
    TAlpha(typeId)
  }

  def reset(): Unit = {
    typeId = 0
    holeId = 0
  }
}

object Helpers {

  import FreshGen._
  import Substitution._

  def gen(typ: Type)(implicit subst: TypeSubst): Value = typ match {
    case TInt => VNum(10) //TODO(Change this to be random)
    case TBool => VBool(scala.util.Random.nextBoolean)
    case TTuple(t1, t2) => VTuple(gen(t1), gen(t2))
    // TODO(rachit): This might be wrong
    case TFun => VLambda("x", EVal(freshHole(freshType())), Map())
    case TTree(t) => scala.util.Random.nextGaussian match {
      case n if n <= 0 => Leaf(t)
      case _ => {
        Node(t, freshHole(t), freshHole(TTree(t)), freshHole(TTree(t)))
      }
    }
    case a@TAlpha(_) => subst.get(a) match {
      case Some(t) => gen(t)
      case None => freshHole(a)
    }
  }

  def n_fail(v: Value, t: Type, sig: ValSubst, theta: TypeSubst):
    (Option[Value], ValSubst, TypeSubst) = {
    (v, t) match {
      case (a@VHole(_, alpha), t) => {
        sig.get(a) match {
          case Some(v) => {
            val thetaPrime = unify(typeOf(v), t, theta)
            (Some(v), sig, unify(t, alpha, thetaPrime))
          }
          case None => {
            val theta1 = unify(alpha, t, theta)
            val genV = gen(t)(theta1)
            (Some(genV), compose(a, genV, sig), theta1)
          }
        }
      }
      case (n@VNum(_), TInt) => (Some(n), sig, theta)
      case (b@VBool(_), TBool) => (Some(b), sig, theta)
      case (l@VLambda(_, _, _), TFun) => (Some(l), sig, theta)
      case (t@VTuple(v1, v2), TTuple(t1, t2)) => {
        val theta1 = unify(typeOf(v1), t1, theta)
        val theta2 = unify(typeOf(v2), t2, theta1)
        (Some(t), sig, theta2)
      }
      case (l@Leaf(t1), TTree(t2)) => {
        val theta1 = unify(t1, t2, theta)
        (Some(l), sig, theta1)
      }
      case (n@Node(t1, _, _, _), TTree(t2)) => {
        val theta1 = unify(t1, t2, theta)
        (Some(n), sig, theta1)
      }
      case p => {
        // println(s"got $p")
        (None, sig, theta)
      }
    }
  }

  def narrow(v: Value, t: Type, s: ValSubst, th: TypeSubst):
    (Option[Value], ValSubst, TypeSubst) = {
      // println(Console.CYAN + s"narrow called with $v, $t, $th" + Console.RESET)
      try { n_fail(v, t, s, th) } catch {
        case UnificationError(m) => {
          // println(m)
          (None, s, th)
        }
      }
    }

  def typeOf(v: Value): Type = v match {
    case VNum(_) => TInt
    case VBool(_) => TBool
    case VLambda(_, _, _) => TFun
    case VTuple(t1, t2) => TTuple(typeOf(t1), typeOf(t2))
    case Leaf(t) => TTree(t)
    case Node(t, _, _, _) => TTree(t)
    case VHole(_, t) => t
  }

  case class UnificationError(msg: String) extends Exception(msg)

  def unify(t1: Type, t2: Type, subst: TypeSubst): TypeSubst = {
    if (t1 == t2) subst
    else {
      (t1, t2) match {
        case (a@TAlpha(_), b@TAlpha(_)) => compose(a, b, subst)
        case (a@TAlpha(_), typ) => compose(a, typ, subst)
        case (typ, a@TAlpha(_)) => compose(a, typ, subst)
        case (TTuple(l1, r1), TTuple(l2, r2)) => {
          val substPrime = unify(l1, l2, subst)
          unify(l2, r2, substPrime)
        }
        case (TTree(t1), TTree(t2)) => unify(t1, t2, subst)
        case _  => throw new UnificationError(s"Tried to unify $t1 and $t2")
      }
    }
  }
}
