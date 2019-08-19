object LastDigitSumSquareOfFibNumber {
  def main(args: Array[String]): Unit = {
    println(lastDigitSumSquareOfFibNumber(1234567890))

  }


  def lastDigitSumSquareOfFibNumber(k: Long): Long = {

    val pisanoPeriodArr = PisanoPeriod.PisanoPeriod(10)
    val currentDigit = pisanoPeriodArr(modulo(k.toInt, pisanoPeriodArr.length))
    val nextDigit = pisanoPeriodArr(modulo(k.toInt + 1, pisanoPeriodArr.length))
    modulo(currentDigit * nextDigit, 10)


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
