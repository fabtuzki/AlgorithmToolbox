object MaximumAdRevenue extends App {

  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val profit = input(1).split(" ").map(_.toInt)
    val slot = input(2).split(" ").map(_.toInt)
    println(maximumAdRevenue(profit, slot))
  }


  def maximumAdRevenue(profit: Array[Int], slot: Array[Int]): Long = {
    val sortedProfit = profit.sorted
    val sortedSlot = slot.sorted
    var output = 0L
    for (i <- 0 until sortedProfit.length) {
      output += sortedProfit(i) * sortedSlot(i)

    }
    output
  }

}
