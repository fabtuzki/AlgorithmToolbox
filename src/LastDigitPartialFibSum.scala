object LastDigitPartialFibSum {

  def main(args: Array[String]): Unit = {
    println(lastDigitPartialFibSum(10, 200))
  }


  def lastDigitPartialFibSum(s: Long, e: Long): Long = {
    val pisanoPeriodArr = PisanoPeriod.PisanoPeriod(10)
    val currentDigit = pisanoPeriodArr.slice(modulo(s.toInt, pisanoPeriodArr.length), modulo(e.toInt + 1, pisanoPeriodArr.length)).sum

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
