import scala.util.control.Breaks

object MaximumValueOfLoot extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray

    val knapsack = input(0).split(" ")(1).toLong
    val weight = input.drop(1).map(_.split(" ")).map(x => (x(0).toLong, x(1).toLong))

    //    val weight = Array((10000L, 1215124L), (3123L, 24142224L), (123124L, 123191L), (1000L, 1L), (1231241L, 12L), (1412L, 123L))
    //    val knapsack = 30

    println(maximumValueOfLoot(weight, knapsack))

  }


  def maximumValueOfLoot(item: Array[(Long, Long)], knapsack: Long): Double = {
    var finalValue = 0D
    val itemSorted = item.sortBy(x => (x._1.toDouble / x._2))
    //    println("item after sorted " + itemSorted.mkString(" ") + " value of each item " + itemSorted.map(x => (x._1.toDouble / x._2)).mkString(" "))
    //the left over weight:
    var leftWeight = knapsack
    val loop = new Breaks
    loop.breakable {
      for (i <- (0 until itemSorted.length).reverse) {
        if (leftWeight == 0L) {
          loop.break()
        } else if (leftWeight > 0 && leftWeight >= itemSorted(i)._2) {
          finalValue += itemSorted(i)._1
          leftWeight -= itemSorted(i)._2
        } else if (leftWeight > 0 && leftWeight < itemSorted(i)._2) {
          finalValue += (itemSorted(i)._1.toDouble / itemSorted(i)._2) * leftWeight
          leftWeight = 0L
        }
      }
    }
    finalValue
  }
}
