package dwit

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}
/*


*/

class Parse extends RegexParsers with PackratParsers {
  import Syntax._

  type P[A] = PackratParser[A]

  lazy val number: P[Int] = """[-]?\d+""".r ^^ (x => x.toInt)

  lazy val bool: P[Boolean] = (
    "true" ^^ (_ => true) | "false" ^^ (_ => false)
  )

  val keys = Set(
    "if", "then", "else", "let", "in", "false", "true", "and", "or", "fix", "rec", "match", "fst",
    "snd", "with"
  )
  def notKeyword: PartialFunction[String, String] = {
    case id if keys.contains(id) == false => id
  }

  lazy val _id: P[Id] = """[_a-zA-Z][_0-9a-zA-Z]*""".r ^^ (x => x)
  lazy val id = _id ^? (notKeyword, (id => s"$id is a reserved keyword"))
  lazy val varId: P[Expr] = id ^^ (x => EVar(x))

  lazy val tuple: P[Expr] = "(" ~> expr ~ "," ~ expr <~ ")" ^^ { case l ~ _ ~ r => ETuple(l, r) }

  lazy val atom: P[Expr] = (
    number        ^^ (n => EVal(VNum(n)))       |
    bool          ^^ (b => EVal(VBool(b)))      |
    varId                                       |
    tuple                                       |
    "[]"          ^^ (_ => ENil)                |
    "(" ~> expr <~ ")"
  )

  lazy val app: P[Expr] = (
    app ~ atom  ^^ { case a ~ as => EApp(a, as) } |
    "fst" ~> atom ^^ { case a => ECaseOfProduct(a, List("_f", "_s"), EVar("_f")) } |
    "snd" ~> atom ^^ { case a => ECaseOfProduct(a, List("_f", "_s"), EVar("_s")) } |
    atom
  )

  lazy val list: P[Expr] = (
    app ~ "::" ~ list ^^ { case h ~ _ ~ t => ECons(h, t) } |
    app
  )
  lazy val mul: P[Expr] =  (
    mul ~ "*" ~ app ^^ { case l ~ _ ~ r => EBinOp("mul", l, r) } |
    mul ~ "/" ~ app ^^ { case l ~ _ ~ r => EBinOp("div", l, r) } |
    mul ~ "%" ~ app ^^ { case l ~ _ ~ r => EBinOp("mod", l, r) } |
    list
  )

  lazy val add: P[Expr] =  (
    add ~ "+" ~ mul ^^ { case l ~ _ ~ r => EBinOp("add", l,r) } |
    add ~ "-" ~ mul ^^ { case l ~ _ ~ r => EBinOp("sub", l, r) } |
    mul
  )

  lazy val cmp: P[Expr] =  (
    add ~ ">" ~ add ^^ { case l ~ _ ~ r => EBinOp("gt",l,r) }  |
    add ~ "<" ~ add ^^ { case l ~ _ ~ r => EBinOp("lt",l,r) }  |
    add ~ "=" ~ add ^^ { case l ~ _ ~ r => EBinOp("eq",l,r) } |
    add
  )

  lazy val and: P[Expr] =  (
    and ~ "and" ~ cmp ^^ { case l ~ _ ~ r => EITE(r, l, r) } |
    cmp
  )

  lazy val or: P[Expr] =  (
    or ~ "or" ~ and ^^ { case l ~ _ ~ r => EITE(l, l, r) }  |
    and
  )

  lazy val matchList: P[Expr] = (
    "match" ~> expr ~ ("with" ~ "|" ~ "[]" ~ "->" ~> expr) ~ "|" ~ id ~ "::" ~ id ~ "->" ~ expr ^^ {
      case e ~ lb ~ _ ~ b1 ~ _ ~ b2 ~ _ ~ nb => ECaseOfTree(e, lb, List(b1, b2), nb)
    }
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
    "let" ~ "(" ~> id ~ "," ~ id ~ ")" ~ "=" ~ expr ~ "in" ~ expr ^^ {
      case b1 ~ _ ~ b2 ~ _ ~ _ ~ tup ~ _ ~ body => ECaseOfProduct(tup, List(b1, b2), body)
    } |
    matchList |
    or
  )
}

object Parser {
  private val parser = new Parse()
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
