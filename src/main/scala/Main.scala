package dwit

object Main extends App {
  import scopt.OptionDef

  case class Config(path: String, debugLevel: Int = 0, bound: Int = 1000)

  val parser = new scopt.OptionParser[Config]("dynwit") {
    head("dynwit", "0.1")

    cmd("witness").
    text("generate a witness for a given program").
    children(
      opt[String]('f', "file").
        required.
        action( (x, c) => c.copy(path = x) ).
        text("file path must be given as a string"),

      opt[Int]('b', "bound").
        optional.
        action( (x, c) => c.copy(bound = x.toInt) ).
        text("bound must be given as a int"),

      opt[Int]('d', "debug").
        optional.
        unbounded.
        action((x, c) => c.copy(debugLevel = x.toInt)).
        validate( x =>
            if (Set(0, 1, 2).contains(x)) success
            else failure("Value <debug> must be 0, 1 or 2")
        ).
        text(
          "Defines the amount of data to be printed. 0: Default/minimum, 1: Tracing, 2: Debuggin"
        )
    )
  }

  parser.parse(args, Config(path = ".")) match {
    case None => {
      System.exit(0)
    }
    case Some(config) => {
      import Parser._
      import Witness._
      import java.nio.file.NoSuchFileException

      try {
        val w = findWitness(parseFile(config.path), config.debugLevel, config.bound)
        println(w)
      } catch {
        case e: NoSuchFileException => println(e.getFile + " is not a valid file path")
      }
    }

  }
}

