object Fibonacci {
  def main(args: Array[String]): Unit = {
    println(Fibonacci(50))
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


