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

  def prettyExpr(e: Expr): Doc = e match {
    case EVal(v) => v.toString
    case EVar(id) => text(id)
    case EFun(id, body) => text("fun") <+> id <+> text("->") <+>  prettyExpr(body)
    case EFix(fn, fb) => text("fix") <+> fn <+> text("->") <+> prettyExpr(fb)
    case EApp(e1, e2) => parens(prettyExpr(e1)) <+> parens(prettyExpr(e2))
    case EAdd(op, e1, e2) => prettyExpr(e1) <+> opToString(op) <+> prettyExpr(e2)
    case EITE(p, c, a) => {
      "if" <+> prettyExpr(p) <+> "then" <+> prettyExpr(c) <+> "else" <+> prettyExpr(a)
    }
    case ETuple(e1, e2) => parens(prettyExpr(e1) <> comma <+> prettyExpr(e2))
    case ECaseOfProduct(e, bs, b) => {
      "match" <+> prettyExpr(e) <+> "with" <+> parens(bs.mkString(", ")) <+> "->" <+> prettyExpr(b)
    }
    case ECaseOfTree(e, lb, bs, b) => {
      "match" <+> prettyExpr(e) <+> "with" <@>
      nest("| [] ->" <+> prettyExpr(lb)) <@>
      nest("| " <> bs.mkString("::") <+> "->" <+> prettyExpr(b))
    }
    case node@ECons(e1, e2) => prettyExpr(e1) <+> colon <> colon <+> prettyExpr(e2)
    case ENil => text("[]")
  }

  def pretty(expr: Expr): String = layout(prettyExpr(expr))
}
