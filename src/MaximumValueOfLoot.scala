import scala.util.control.Breaks

object MaximumValueOfLoot extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray

    val knapsack = input(0).split(" ")(1).toInt
    val weight = input.drop(1).map(_.split(" ")).map(x => (x(0).toInt, x(1).toInt))
    println(maximumValueOfLoot(weight, knapsack))

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
