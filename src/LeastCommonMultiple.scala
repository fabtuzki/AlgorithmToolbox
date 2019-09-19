object LeastCommonMultiple extends  App{
 override def main(args: Array[String]): Unit = {
   val input = scala.io.Source.stdin.getLines().next().split(" ").map(_.toInt)

   println(LeastCommonMultiple(input(0), input(1)))
  }


  def LeastCommonMultiple(a: Int, b: Int): Long = {
    (a.toLong * b) / GreatestCommonDivisor(a, b)
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
