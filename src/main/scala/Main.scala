package dwit
import GlobalConfig._

object Main extends App {
  import scopt.OptionDef

  case class Config(
    mode: String, path: String, args: Seq[String], debugLevel: Int = 0, bound: Int = 1000
  )

  val parser = new scopt.OptionParser[Config]("dynwit") {
    head("dynwit", "0.1")

    cmd("witness").
    action((_, c) => c.copy(mode = "witness")).
    text("generate a witness for a given program").
    children(
      opt[String]('f', "file").
        required.
        action( (x, c) => c.copy(path = x) ).
        text("file path must be given as a string"),

      opt[Int]('b', "bound").
        optional.
        action( (x, c) => c.copy(bound = x.toInt) ).
        text("Number of traces the to be considered before giving up"),

      opt[Int]('d', "debug").
        optional.
        unbounded.
        action((x, c) => c.copy(debugLevel = x.toInt)).
        validate( x =>
            if (Set(0, 1, 2).contains(x)) success
            else failure("Value <debug> must be 0, 1 or 2")
        ).
        text(
          "Defines the amount of data to be printed."
        )
    )

    note("0: Disable logging (default), 1: generate trace, 2: Debugging mode.\n")

    cmd("eval").
    action((_, c) => c.copy(mode = "eval")).
    text("generate a witness for a given program").
    children(
      opt[String]('f', "file").
        required.
        action( (x, c) => c.copy(path = x) ).
        text("file path must be given as a string"),

      opt[Seq[String]]('a', "args").
        required.
        action( (x, c) => c.copy(args = x) ).
        text("file path must be given as a string"),

      opt[Int]('d', "debug").
        optional.
        unbounded.
        action((x, c) => c.copy(debugLevel = x.toInt)).
        validate( x =>
            if (Set(0, 1, 2).contains(x)) success
            else failure("Value <debug> must be 0, 1 or 2")
        ).
        text("Defines the amount of data to be printed.")
    )
    note("0: Disable logging (default), 1: generate trace, 2: Debugging mode.\n")
  }

  parser.parse(args, Config(args = Seq(), mode = "", path = "")) match {
    case None => {
      System.exit(0)
    }
    case Some(config) => {
      import Parser._
      import java.nio.file.NoSuchFileException

      try {
        GlobalConfig.debugLevel = config.debugLevel

        config.mode match {
          case "witness" => {
            import Witness._
            val w = findWitness(parseFile(config.path), config.bound)
            success(s"The function fails on the following arguments (in-order): ${w.mkString(",")}")
          }
          case "eval" => {
            import Evaluation._
            import Syntax._
            val args = config.args.map(parse)
            val prog = args.foldLeft(parseFile(config.path))((acc, a) => EApp(acc, a))
            try {
              val res = eval(prog)
              success(s"Result was: $res")
            } catch {
              case e: Throwable => {
                failure(s"Evaluation failed. Run with -d 1 to generate the trace")
              }
            }
          }
        }

      } catch {
        case e: NoSuchFileException => println(e.getFile + " is not a valid file path")
      }
    }
  }
}

