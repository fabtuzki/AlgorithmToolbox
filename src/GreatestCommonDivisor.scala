object GreatestCommonDivisor {
  def main(args: Array[String]): Unit = {
    println(GreatestCommonDivisor(28851538, 1183019))

  }

  def GreatestCommonDivisor(a: Int, b: Int): Int = {
    if (modulo(a, b) == 0) {
      b
    } else {
      GreatestCommonDivisor(b, modulo(a, b))
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
