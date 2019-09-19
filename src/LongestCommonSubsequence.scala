object LongestCommonSubsequence extends App {
  override def main(args: Array[String]): Unit = {


    val input = scala.io.Source.stdin.getLines().toArray
    val sequenceA = input(1).split(" ").map(_.toInt)
    val sequenceB = input(3).split(" ").map(_.toInt)
    //    val sequenceA = Array(2 ,7 ,8 ,3)
    //    val sequenceB = Array(5, 2, 8, 7)

    println(longestCommonSubsequence(sequenceA, sequenceB))

  }

  def longestCommonSubsequence(sequenceA: Array[Int], sequenceB: Array[Int]): Int = {
    //Initiate the matrix:
    val matrixT = Array.ofDim[Int](sequenceA.length + 1, sequenceB.length + 1)

    //fill in base case
    //fill in all row of 0
    for (i <- 0 to sequenceB.length) {
      matrixT(0).update(i, 0)
    }
    //fill in all column of 0
    for (i <- 0 to sequenceA.length) {
      matrixT(i).update(0, 0)
    }

    //Loop through the matrix each row through all column
    for (row <- 1 to sequenceA.length) {
      for (column <- 1 to sequenceB.length) {
        val A = matrixT(row - 1)(column - 1) //case match/unmatch
        val B = matrixT(row - 1)(column) //case skip
        val C = matrixT(row)(column - 1) //case skip
        var pointOfMatch = A
        //case match
        if (sequenceA(row - 1) == sequenceB(column - 1)) {
          pointOfMatch += 1
        }
        val finalValue = Array(pointOfMatch, B, C).max
        matrixT(row).update(column, finalValue)
      }
    }


    //    println("matrix of matching ")
    //    matrixT.foreach(x => println(x.mkString(" ")))
    matrixT(sequenceA.length)(sequenceB.length)

  }
}
