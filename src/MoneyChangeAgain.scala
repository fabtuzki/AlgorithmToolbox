
object MoneyChangeAgain extends App {
  override def main(args: Array[String]): Unit = {

    val input = scala.io.Source.stdin.getLines().next().toInt
    println(moneyChangeDP(input))

  }

  def moneyChangeDP(change: Int): Int = {
    val coinArr = Array(1, 3, 4)
    val minNumberOfCoins = Array.ofDim[Int](change + 1)
    minNumberOfCoins.update(0, 0)

    for (m <- 1 to change) {
      minNumberOfCoins.update(m, 1000000000)

      for (i <- 0 until coinArr.length) {
        if (m >= coinArr(i)) {
          val numCoins = minNumberOfCoins(m - coinArr(i)) + 1
          if (numCoins < minNumberOfCoins(m)) {
            minNumberOfCoins.update(m, numCoins)
          }
        }
      }
    }
    //    println("save array: " + minNumberOfCoins.mkString(" "))
    minNumberOfCoins(change)
  }


}
