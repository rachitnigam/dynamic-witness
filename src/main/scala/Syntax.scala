package dwit
object Syntax {

  type Id = String
  type TypeId = Int
  type HoleId = Int

  type TypeSubst = Map[TAlpha, Type]
  type ValSubst = Map[VHole, Value]
  type Env = Map[Id, Value]

  sealed trait Expr {
    override def toString: String = this match {
      case EVal(c) => c.toString
      case EVar(id) => id
      case EFun(id, b) => s"(fun $id -> $b)"
      case EFix(fn, fb) => s"fix $fn -> $fb"
      case EApp(e1, e2) => s"($e1 $e2)"
      case EAdd(_, e1, e2) => s"$e1 add $e2"
      case ECmp(_, e1, e2) => s"$e1 cmp $e2"
      case EITE(p, c, a) => s"if $p then $c else $a"
      case ETuple(e1, e2) => s"($e1, $e2)"
      case ELeaf => "leaf"
      case ENode(e1, e2, e3) => s"node($e1, $e2, $e3)"
      case ECaseOfProduct(e, bind, b) => s"case $e of ${bind.mkString(",")} => $b"
      case ECaseOfTree(e, lb, bind, b) => {
        s"case $e of\nleaf => $lb\nnode ${bind.mkString(",")} => $b"
      }
    }
  }
  case class EVal(v: Value) extends Expr
  case class EVar(id: Id) extends Expr
  case class EFun(id: Id, body: Expr) extends Expr
  case class EFix(fName: Id, func: Expr) extends Expr
  case class EApp(e1: Expr, e2: Expr) extends Expr
  case class EAdd(op: (Int, Int) => Int, e1: Expr, e2: Expr) extends Expr
  case class ECmp(op: (Int, Int) => Boolean, e1: Expr, e2: Expr) extends Expr
  case class EITE(p: Expr, c: Expr, a: Expr) extends Expr
  case class ETuple(e1: Expr, e2: Expr) extends Expr
  case class ECaseOfProduct(e: Expr, bind: List[Id], body: Expr) extends Expr
  case class ECaseOfTree(e: Expr, leafBody: Expr, bind: List[Id], body: Expr) extends Expr
  case class ENode(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case object ELeaf extends Expr

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


  def cons(hd: Expr, tl: Expr) = ENode(hd, tl, ELeaf)

  sealed trait Value {
    override def toString: String = this match {
      case VNum(i) => i.toString
      case VBool(b) => b.toString
      case VTuple(t1, t2) => s"($t1, $t2)"
      case VLambda(id, b, e) => s"(λ$id. $b, env: $e)"
      case VHole(i, t) => s"[$i, $t]"
      case Leaf(t) => s"leaf[$t]"
      case Node(t, t1, t2, t3) => s"node[$t]($t1, $t2, $t3)"
    }
  }
  case class VNum(i: Int) extends Value
  case class VBool(b: Boolean) extends Value
  case class VTuple(v1: Value, v2: Value) extends Value
  case class VLambda(id: Id, body: Expr, closure: Env) extends Value
  case class VHole(ident: HoleId, typ: Type) extends Value
  case class Node(t: Type, v1: Value, v2: Value, v3: Value) extends Value
  case class Leaf(t: Type) extends Value

  sealed trait Type {
    override def toString: String = this match {
      case TInt => "int"
      case TBool => "bool"
      case TFun => s"fun"
      case TTuple(t1, t2) => s"($t1, $t2)"
      case TTree(t) => s"tree $t"
      case TAlpha(i) => s"\'${(97 + i).toChar}"
    }
  }
  case object TInt extends Type
  case object TBool extends Type
  case object TFun extends Type
  case class TTuple(e1: Type, e2: Type) extends Type
  case class TTree(t: Type) extends Type
  case class TAlpha(ident: TypeId) extends Type

}
