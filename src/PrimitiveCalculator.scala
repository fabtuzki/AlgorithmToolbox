import scala.collection.mutable.ArrayBuffer

object PrimitiveCalculator extends App {
  override def main(args: Array[String]): Unit = {

    val input = scala.io.Source.stdin.getLines().next().toInt
    val (numberOfOperation, backTrackArr) = primitiveCalculator(input)
    println(numberOfOperation)
    println(unwind(numberOfOperation, backTrackArr).mkString(" "))

  }

  def unwind(numberOfOperation: Int, backTrackArray: Array[Int]): Array[Int] = {
    val output = new ArrayBuffer[Int]
    var i = backTrackArray.length - 1
    output.append(backTrackArray.length - 1)
    while (backTrackArray(i) > 0) {
      output.append(backTrackArray(i))
      i = backTrackArray(i)
    }
    output.toArray.reverse
  }

  def primitiveCalculator(n: Int): (Int, Array[Int]) = {
    val pc = Array.ofDim[Int](n + 1)
    pc.update(1, 0)
    val backTrackArr = Array.ofDim[Int](n + 1)
    backTrackArr.update(1, -1)
    for (m <- 2 to n) {
      pc.update(m, 1000000000)
      if (m - 1 > 0) {
        //check number of operation here
        val numberOfOperation = pc(m - 1) + 1
        if (pc(m) > numberOfOperation) {
          pc.update(m, numberOfOperation)
          backTrackArr.update(m, m - 1)
        }

      }

      if (modulo(m, 2) == 0 && m / 2 > 0) {
        val numberOfOperation = pc(m / 2) + 1
        if (pc(m) > numberOfOperation) {
          pc.update(m, numberOfOperation)
          backTrackArr.update(m, m / 2)
        }

      }

      if (modulo(m, 3) == 0 && m / 3 > 0) {
        val numberOfOperation = pc(m / 3) + 1
        if (pc(m) > numberOfOperation) {
          pc.update(m, numberOfOperation)
          backTrackArr.update(m, m / 3)
        }
      }
    }

    (pc(n), backTrackArr)
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
