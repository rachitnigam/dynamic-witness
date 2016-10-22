package dwit
object Syntax {

  type Id = String
  type TypeId = Int
  type HoleId = Int

  type TypeSubst = Map[TAlpha, Type]
  type ValSubst = Map[VHole, Value]

  sealed trait Expr
  case class EVal(v: Value) extends Expr
  case class EVar(id: Id) extends Expr
  case class EApp(e1: Expr, e2: Expr) extends Expr
  case class EAdd(e1: Expr, e2: Expr) extends Expr
  case class EITE(p: Expr, c: Expr, a: Expr) extends Expr
  case class ETuple(e1: Expr, e2: Expr) extends Expr
  case class ECaseOfProduct(e: Expr, bind: List[Id], body: Expr) extends Expr
  case class ECaseOfTree(e: Expr, leafBody: Expr, bind: List[Id], body: Expr) extends Expr
  case class ENode(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case object ELeaf extends Expr

  sealed trait Value
  case class VNum(i: Int) extends Value
  case class VBool(b: Boolean) extends Value
  case class VTuple(v1: Value, v2: Value) extends Value
  case class VLambda(id: Id, body: Expr) extends Value
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
