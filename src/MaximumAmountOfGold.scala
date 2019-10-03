object MaximumAmountOfGold extends App {
  override def main(args: Array[String]): Unit = {

    val input = scala.io.Source.stdin.getLines().toArray

    val weight = input(0).split(" ")(0).toInt
    val goldbar = input(1).split(" ").map(_.toInt)
    println(maximumAmountOfGold(weight, goldbar))
  }

  def maximumAmountOfGold(weight: Int, goldBar: Array[Int]): Int = {
    //initiate the array of result:
    val matrixT = Array.ofDim[Int](goldBar.length + 1, weight + 1)

    //initiate base case
    for (column <- 0 to weight) {
      matrixT(0).update(column, 0)
    }

    for (row <- 0 to goldBar.length) {
      matrixT(row).update(0, 0)
    }

    //Start looping through the array
    for (row <- 1 to goldBar.length) {
      for (column <- 1 to weight) {
        var maxValueWithoutMostRecentItem = 0

        if (column - goldBar(row - 1) >= 0) {
          maxValueWithoutMostRecentItem += matrixT(row - 1)(column - goldBar(row - 1))
          //update the max value without most recent item in the case of column - row >= value of gold bar just eliminated
          if (column - matrixT(row - 1)(column - goldBar(row - 1)) >= goldBar(row - 1)) {
            maxValueWithoutMostRecentItem += goldBar(row - 1)
          }
        }
        val maxValueAtWeightOfPreviousItem = matrixT(row - 1)(column)
        val finalValue = Array(maxValueWithoutMostRecentItem, maxValueAtWeightOfPreviousItem).max
        //        println("final value at row " + row + " and column " + column + " is " + finalValue)
        matrixT(row).update(column, finalValue)
      }
    }

    //    println("print the matrix")
    //    matrixT.foreach(x => println(x.mkString(" ")))
    matrixT(goldBar.length)(weight)

  }
}
