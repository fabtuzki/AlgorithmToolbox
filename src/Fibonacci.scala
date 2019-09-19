object Fibonacci extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next()

    println(Fibonacci(input.toInt))
  }

  def Fibonacci(n: Int): Long = {
    def FibonacciAccum(ind: Int, prev: Long, next: Long): Long = {
      if (ind == 1) {
        //base case 1:
        return next
      } else if (ind == 0) {
        //base case 0 (for fun, never gonna use ):
        return prev
      } else {
        FibonacciAccum(ind - 1, next, prev + next)
      }
    }

    FibonacciAccum(n, 0, 1)
  }

}


