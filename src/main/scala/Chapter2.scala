package fpis.ch1

object Ch1Exercises {
  //2.1
  def fib(n: Int) = {
    def fib(n: Int, a: Int) = {
      n match {
        case 0 => a
        case 1 => a + 1
        case n => fib(n - 1, a + n)
      }
    }
    fib(n, 0)
  }
}