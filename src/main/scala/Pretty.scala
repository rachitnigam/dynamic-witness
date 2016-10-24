package dwit
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

object Pretty extends PrettyPrinter {

  import Syntax._

  def opToString(fn: String): Doc = fn match {
    case "add" => "+"
    case "sub" => "-"
    case "mul" => "*"
    case "div" => "/"
    case "eq" => "="
    case "lt" => "<"
    case "gt" => ">"
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
    case EAdd(op, e1, e2) => prettyExpr(e1) <+> opToString(op) <+> prettyExpr(e2)
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

  def pretty(expr: Expr): String = layout(prettyExpr(expr))
}
