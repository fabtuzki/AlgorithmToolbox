object LastDigitOfLargeFib extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next()
   println(LastDigit(input.toInt))
  }


  def LastDigit(n: Int): Int = {

    var lastDigit = 0
    var prev = 0
    var next = 1
    if (n == 0) {
      0
    } else if (n == 1) {
      1
    } else {
      for (i <- 2 to n) {
        val tmpNext = modulo(prev + next, 10)
        prev = modulo(next, 10)
        next = modulo(tmpNext, 10)
        lastDigit = modulo(next, 10)
      }
      lastDigit
    }
  }

  def modulo(dividend: Double, divisor: Long): Int = {
    if (dividend < divisor) {
      dividend.toInt
    } else {
      var remainder = 0D
      val estimatedQ = math.floor(dividend / divisor).toLong
      remainder = dividend - estimatedQ * divisor
      while (remainder >= divisor) {
        remainder = remainder - divisor
      }

      remainder.toInt

    }
  }

}
