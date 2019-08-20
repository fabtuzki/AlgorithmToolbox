import scala.util.control.Breaks

object MaximumValueOfLoot {
  def main(args: Array[String]): Unit = {
    println(maximumValueOfLoot(Array((500,  30)), 10))

  }


  def maximumValueOfLoot(item: Array[(Int, Int)], knapsack: Int): Double = {
    var finalValue = 0D
    val itemSorted = item.sortBy(x => x._1 / x._2)
    var leftWeight = knapsack
    val loop = new Breaks
    loop.breakable {
      for (i <- (0 until itemSorted.length).reverse) {
        if (leftWeight == 0) {
          loop.break()
        }
        if (leftWeight >= itemSorted(i)._2) {
          finalValue += itemSorted(i)._1
          leftWeight -= itemSorted(i)._2
        } else {
          finalValue += (itemSorted(i)._1.toDouble / itemSorted(i)._2) * leftWeight
          leftWeight = 0

        }


      }
    }
    finalValue
  }
}
