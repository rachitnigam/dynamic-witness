package dwit

object Syntax {

  type Id = String
  type TypeId = Int
  type HoleId = Int

  type Env = Map[Id, Value]

  sealed trait Expr {
    override def toString: String = Pretty.pretty(this)
  }
  case class EVal(v: Value) extends Expr
  case class EVar(id: Id) extends Expr
  case class EFun(id: Id, body: Expr) extends Expr
  case class EFix(fName: Id, func: Expr) extends Expr
  case class EApp(e1: Expr, e2: Expr) extends Expr
  case class EBinOp(op: String, e1: Expr, e2: Expr) extends Expr
  case class EITE(p: Expr, c: Expr, a: Expr) extends Expr
  case class ETuple(e1: Expr, e2: Expr) extends Expr
  case class ECaseOfProduct(e: Expr, bind: List[Id], body: Expr) extends Expr
  case class ECaseOfTree(e: Expr, leafBody: Expr, bind: List[Id], body: Expr) extends Expr
  case class ECons(e1: Expr, e2: Expr) extends Expr
  case object ENil extends Expr

  def let(id: Id, v: Expr, body: Expr) = {
    EApp(EFun(id, body), v)
  }

  def recFun(fName: Id, args: List[Id], body: Expr) = {
    EFix(fName, mkMultiArgsFun(args, body))
  }

  def mkFun(id: Id, b: Expr): Expr = EFun(id, b)

  def mkMultiArgsFun(args: List[Id], body: Expr): Expr = {
    args.foldRight(body)({ case (id, acc) => mkFun(id, acc) })
  }

  sealed trait Value {
    override def toString: String = this match {
      case VNum(i) => i.toString
      case VBool(b) => b.toString
      case VTuple(t1, t2) => s"($t1, $t2)"
      case VLambda(id, b, e) => s"(λ$id. $b, env: $e)"
      case VHole(i, t) => s"[$i, $t]"
      case VNil(_) => s"[]"
      case VCons(_, t1, t2) => s"$t1::$t2"
    }
  }
  case class VNum(i: Int) extends Value
  case class VBool(b: Boolean) extends Value
  case class VTuple(v1: Value, v2: Value) extends Value
  case class VLambda(id: Id, body: Expr, closure: Env) extends Value
  case class VHole(ident: HoleId, typ: Type) extends Value
  case class VCons(t: Type, v1: Value, v2: Value) extends Value
  case class VNil(t: Type) extends Value

  sealed trait Type {
    override def toString: String = this match {
      case TInt => "int"
      case TBool => "bool"
      case TFun => s"fun"
      case TTuple(t1, t2) => s"($t1, $t2)"
      case TList(t) => s"list $t"
      case TAlpha(i) => s"\'${(96 + i).toChar}"
    }
  }
  case object TInt extends Type
  case object TBool extends Type
  case object TFun extends Type
  case class TTuple(e1: Type, e2: Type) extends Type
  case class TList(t: Type) extends Type
  case class TAlpha(ident: TypeId) extends Type
}
