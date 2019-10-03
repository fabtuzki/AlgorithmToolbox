import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

object LastDigitSumSquareOfFibNumber extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next().toLong
    println(lastDigitSumSquareOfFibNumber(input))

  }


  def lastDigitSumSquareOfFibNumber(k: Long): Long = {

    val pisanoPeriodArr = PisanoPeriod(10)
    val currentDigit = pisanoPeriodArr(modulo(k, pisanoPeriodArr.length))
    val nextDigit = pisanoPeriodArr(modulo(k + 1, pisanoPeriodArr.length))
    modulo(currentDigit * nextDigit, 10)


  }

  def PisanoPeriod(m: Long): Array[Long] = {
    val pisanoPeriod = new ArrayBuffer[Long]
    pisanoPeriod.append(0)
    pisanoPeriod.append(1)
    var modOfFib = 0L
    var prev = 0L
    var next = 1L
    var i = true
    while (i) {

      val tmpNext = modulo(prev + next, m)
      prev = modulo(next, m)
      next = modulo(tmpNext, m)
      modOfFib = modulo(next, m)
      pisanoPeriod.append(modOfFib)

      var checkRepeat = false

      if (pisanoPeriod(pisanoPeriod.length - 1) == 1 && pisanoPeriod(pisanoPeriod.length - 2) == 0 && pisanoPeriod.length > 2) {
        checkRepeat = true
        val currentLength = pisanoPeriod.length
        val loop = new Breaks
        loop.breakable {
          for (k <- 2 until currentLength - 2) {

            val tmpNext = modulo(prev + next, m)
            prev = modulo(next, m)
            next = modulo(tmpNext, m)
            modOfFib = modulo(next, m)
            pisanoPeriod.append(modOfFib)

            if (modOfFib != pisanoPeriod(k)) {
              checkRepeat = false
              loop.break()
            }
          }
        }
      }

      if (checkRepeat) {
        i = false
        pisanoPeriod.trimEnd(pisanoPeriod.length / 2)
      }
    }
    pisanoPeriod.toArray

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
