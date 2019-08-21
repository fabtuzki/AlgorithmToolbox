object MaximumAdRevenue {

  def main(args: Array[String]): Unit = {

    println(maximumAdRevenue(Array(1, 3, -5), Array(-2, 4, 1)))
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
