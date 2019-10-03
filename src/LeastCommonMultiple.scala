object LeastCommonMultiple extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next().split(" ").map(_.toLong)

    println(LeastCommonMultiple(input(0), input(1)))
  }


  def LeastCommonMultiple(a: Long, b: Long): Long = {
    (a.toLong * b) / GreatestCommonDivisor(a, b)
  }

  def GreatestCommonDivisor(a: Long, b: Long): Long = {
    if (modulo(a, b) == 0) {
      b
    } else {
      GreatestCommonDivisor(b, modulo(a, b))
    }
  }

  def modulo(dividend: Double, divisor: Long): Long = {
    if (dividend < divisor) {
      dividend.toLong
    } else {
      var remainder = 0D
      val estimatedQ = math.floor(dividend / divisor).toLong
      remainder = dividend - estimatedQ * divisor
      while (remainder >= divisor) {
        remainder = remainder - divisor
      }

      remainder.toLong

    }
  }

}
