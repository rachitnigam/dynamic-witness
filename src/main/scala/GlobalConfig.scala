package dwit
// Stores the global config here
object GlobalConfig {
  var debugLevel = 0

  def log1[A](thunk: => A): A = {
    val res = thunk
    if(debugLevel == 1) println(res)
    res
  }

  def log2[A](thunk: => A): A = {
    val res = thunk
    if(debugLevel == 2) println(res)
    res
  }

  def error[A <: Throwable](exn: A) = {
    if(debugLevel == 2) println(Console.RED + "[ERROR] " + exn.getMessage() + Console.RESET)
    throw exn
  }

  def success[A](thunk: => A) = {
    val res = thunk
    println(Console.GREEN + "[SUCCESS] " + res + Console.RESET)
    res
  }

  def failure[A](thunk: => A) = {
    val res = thunk
    println(Console.RED + "[FAILURE] " + res + Console.RESET)
    res
  }
}
