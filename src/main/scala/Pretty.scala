package dwit
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

object Pretty extends PrettyPrinter {

  import Syntax._

  def _add: (Int, Int) => Int = (_ + _)
  def _sub: (Int, Int) => Int = (_ - _)
  def _mul: (Int, Int) => Int = (_ * _)
  def _div: (Int, Int) => Int = (_ / _)

  def _eq: (Int, Int) => Boolean = (_ == _)
  def _lt: (Int, Int) => Boolean = (_ < _)
  def _gt: (Int, Int) => Boolean = (_ > _)

  def funToString(fn: (Int, Int) => Int): Doc = fn match {
    case _ if fn == _add => "+"
    case _ if fn == _sub => "-"
    case _ if fn == _mul => "*"
    case _ if fn == _sub => "-"
  }

  def cmpToString(cmp: (Int, Int) => Boolean) = cmp match {
    case _ if cmp == _eq => "="
    case _ if cmp == _lt => "<"
    case _ if cmp == _gt => ">"
  }

  def isList(e: Expr): Boolean = e match {
    case ELeaf => true
    case ENode(_, rest, ELeaf) => isList(rest)
    case _ => false
  }

  def prettyList(e: Expr): Doc = e match {
    case ELeaf => text("[]")
    case ENode(v, rest, ELeaf) => prettyExpr(e) <> colon <> colon <> prettyList(rest)
    case _ => ???
  }

  def prettyExpr(e: Expr): Doc = e match {
    case EVal(v) => v.toString
    case EVar(id) => text(id)
    case EFun(id, body) => text("fun") <+> text("->") <+>  prettyExpr(body)
    case EFix(fn, fb) => text("fix") <+> text("->") <+> prettyExpr(fb)
    case EApp(e1, e2) => parens(prettyExpr(e1)) <+> parens(prettyExpr(e2))
    case EAdd(op, e1, e2) => prettyExpr(e1) <+> funToString(op) <+> prettyExpr(e2)
    case ECmp(op, e1, e2) => prettyExpr(e1) <+> cmpToString(op) <+> prettyExpr(e2)
    case EITE(p, c, a) => {
      "if" <+> prettyExpr(p) <+> "then" <+> prettyExpr(c) <+> "else" <+> prettyExpr(a)
    }
    case ETuple(e1, e2) => parens(prettyExpr(e1) <> comma <+> prettyExpr(e2))
    case ECaseOfProduct(e, bs, b) => ???
    case ECaseOfTree(e, lb, bs, b) => ???
    case node@ENode(e1, e2, e3) => {
      if (isList(node)) prettyList(node)
      else ???
    }
    case ELeaf => ???
  }

  def pretty(expr: Expr): String = prettyExpr(expr).toString
}
