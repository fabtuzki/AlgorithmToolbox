object LastDigitOfFibSum {
  def main(args: Array[String]): Unit = {
    println(lastDigitOfFibSum(3))

  }

  def lastDigitOfFibSum(k: Long): Long = {
    val pisanoPeriodArr = PisanoPeriod.PisanoPeriod(10)
    val currentDigit =  pisanoPeriodArr.slice(0, modulo(k.toInt + 1, pisanoPeriodArr.length) ).sum

    modulo(currentDigit, 10)
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
