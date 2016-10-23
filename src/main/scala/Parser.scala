package dwit

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}
/*


*/

private class ParseQuark extends RegexParsers with PackratParsers {
  import Syntax._

  var boolConst = 0
  def freshBoolConst(): Int = {
    boolConst += 1
    boolConst
  }

  def _add: (Int, Int) => Int = (_ + _)
  def _mul: (Int, Int) => Int = (_ * _)
  def _div: (Int, Int) => Int = (_ / _)
  def _sub: (Int, Int) => Int = (_ - _)

  def _eq: (Int, Int) => Boolean = (_ == _)
  def _lt: (Int, Int) => Boolean = (_ < _)
  def _gt: (Int, Int) => Boolean = (_ > _)

  type P[A] = PackratParser[A]

  lazy val number: P[Int] = """[-]?\d+""".r ^^ (x => x.toInt)

  lazy val bool: P[Boolean] = (
    "true" ^^ (_ => true) | "false" ^^ (_ => false)
  )

  val keys = Set(
    "if", "then", "else", "let", "in", "false", "true", "and", "or", "fix", "rec"
  )
  def notKeyword: PartialFunction[String, String] = {
    case id if keys.contains(id) == false => id
  }

  lazy val _id: P[Id] = """[_a-zA-Z][_0-9a-zA-Z]*""".r ^^ (x => x)
  lazy val id = _id ^? (notKeyword, (id => s"$id encountered"))
  lazy val varId: P[Expr] = id ^^ (x => EVar(x))

  lazy val tuple: P[Expr] = "(" ~> expr ~ "," ~ expr <~ ")" ^^ { case l ~ _ ~ r => ETuple(l, r) }

  lazy val atom: P[Expr] = (
    number        ^^ (n => EVal(VNum(n)))       |
    bool          ^^ (b => EVal(VBool(b)))      |
    varId                                       |
    tuple                                       |
    "(" ~> expr <~ ")"
  )

  lazy val app: P[Expr] = (
    app ~ atom  ^^ { case a ~ as => EApp(a, as) } |
    atom
  )

  lazy val list: P[Expr] = (
    app ~ "::" ~ list ^^ { case h ~ _ ~ t => cons(h, t) } |
    app
  )
  lazy val mul: P[Expr] =  (
    mul ~ "*" ~ app ^^ { case l ~ _ ~ r => EAdd(_mul, l, r) } |
    mul ~ "/" ~ app ^^ { case l ~ _ ~ r => EAdd(_div, l, r) } |
    list
  )

  lazy val add: P[Expr] =  (
    add ~ "+" ~ mul ^^ { case l ~ _ ~ r => EAdd(_add, l,r) } |
    add ~ "-" ~ mul ^^ { case l ~ _ ~ r => EAdd(_sub, l, r) } |
    mul
  )

  lazy val cmp: P[Expr] =  (
    add ~ ">" ~ add ^^ { case l ~ _ ~ r => ECmp(_gt,l,r) }  |
    add ~ "<" ~ add ^^ { case l ~ _ ~ r => ECmp(_lt,l,r) }  |
    add ~ "=" ~ add ^^ { case l ~ _ ~ r => ECmp(_eq,l,r) } |
    add
  )

  lazy val and: P[Expr] =  (
    and ~ "and" ~ cmp ^^ { case l ~ _ ~ r => {
      val nname = "_and" + freshBoolConst().toString
      let(nname, l, EITE(EVar(nname), r, EVar(nname)))
    }} |
    cmp
  )

  lazy val or: P[Expr] =  (
    or ~ "or" ~ and ^^ { case l ~ _ ~ r => {
      val nname = "_or" + freshBoolConst().toString
      let(nname, l, EITE(EVar(nname), EVar(nname), r))
    }}  |
    and
  )

  lazy val expr: P[Expr] = (
    "fun" ~> id ~ "->" ~ expr ^^ { case arg ~ _ ~ b => mkFun(arg, b) }     |
    "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr ^^ { case p ~ _ ~ c ~ _ ~ a => EITE(p, c, a) }|
    "let" ~> id ~ "=" ~ expr ~ "in" ~ expr ^^ { case i ~ _ ~ v ~ _ ~ b => let(i, v, b) }       |
    "let" ~ "rec" ~> id ~ rep1(id) ~ "=" ~ expr ~ "in" ~ expr ^^ {
      case fn ~ args ~ _ ~ fb ~ _ ~ body => let(fn, recFun(fn, args, fb), body)
    } |
    "fix" ~> id ~ "->" ~ expr ^^ { case fn ~ _ ~ fb => EFix(fn, fb) }                          |
    "let" ~> id ~ rep1(id) ~ "=" ~ expr ~  "in" ~ expr ^^ {
      case fName ~ args ~ _ ~ fBody ~ _ ~ body => let(fName, mkMultiArgsFun(args, fBody), body)
    } |
    or
  )
}

object Parser {
  private val parser = new ParseQuark()
  import parser._

  def parse(str: String): Syntax.Expr = parseAll(expr, str.trim) match {
    case Success(r, _) => r
    case m => throw new Exception(s"$m")
  }

  def parseFile(path: String) = {
    import java.nio.file._
    parse(new String(Files.readAllBytes(Paths.get(path))))
  }

}
