object MaximumAdRevenue extends App {

  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val profit = input(1).split(" ").map(_.toLong)
    val slot = input(2).split(" ").map(_.toLong)
    println(maximumAdRevenue(profit, slot))
  }


  def maximumAdRevenue(profit: Array[Long], slot: Array[Long]): String = {
    val sortedProfit = profit.sorted
    val sortedSlot = slot.sorted
    var output = 0D
    for (i <- 0 until sortedProfit.length) {
      output += sortedProfit(i) * sortedSlot(i)

    }
    math.round(output).toString
  }

}
