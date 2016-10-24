package dwit

object Tracing {
  import Evaluation._
  import Syntax._

  def tracingCekLoop(state: State, vSub: ValSubst, tSub: TypeSubst, gr: StringBuilder): String = {
    state._1 match {
      case Left(r) => gr ++= s""""$r""""
      case Right(r) => gr ++= s""""$r""""
    }

    state match {
      case (Right(v), _, Nil) => {
        gr ++= ";\n}"
        gr.toString
      }
      case state => {
        try {
          val (s, vs, ts) = cek(state, vSub, tSub)
          s._1 match {
            case Left(r) => gr ++= s""" -> "$r";"""
            case Right(r) => gr ++= s""" -> "$r";"""
          }
          tracingCekLoop(s, vs, ts, gr)
        } catch {
          case Stuck(_, _, _) => {
            gr ++= ";\n}"
            gr.toString
          }
        }
      }
    }
  }

  def trace(e: Expr) = tracingCekLoop((Left(e), Map(), List()), Map(), Map(), new StringBuilder(
    "strict digraph {\n"))
}
