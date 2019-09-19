object EditDistance extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val stringA = input(0)
    val stringB = input(1)

    println(editDistance(stringA, stringB))

  }


  def editDistance(stringA: String, stringB: String): Int = {
    val arrayA = stringA.toCharArray

    val arrayB = stringB.toCharArray

    val matrixT = Array.ofDim[Int](arrayA.length + 1, arrayB.length + 1)

    //Add base case:

    for (i <- 0 to arrayA.length) {
      matrixT(i).update(0, i)
    }

    for (i <- 0 to arrayB.length) {
      matrixT(0).update(i, i)
    }

    //Loop through all value of each row then next row
    for (row <- 1 to arrayA.length) {
      for (column <- 1 to arrayB.length) {
        //check if value at ArrayA(row) ArrayB(column) is equal to each other
        var caseMatch = 1000000000
        var caseMismatch = 1000000000
        var caseDelete = 1000000000
        var caseInsert = 1000000000
        caseDelete = 1 + matrixT(row - 1)(column)
        caseInsert = 1 + matrixT(row)(column - 1)
        caseMismatch = 1 + matrixT(row - 1)(column - 1)

        if (arrayA(row - 1) == arrayB(column - 1)) {
          caseMatch = matrixT(row - 1)(column - 1)
        }

        val minPoint = Array(caseDelete, caseInsert, caseMatch, caseMismatch).min

        matrixT(row).update(column, minPoint)

      }
    }
    //    println("print the matrix out : ")
    //    matrixT.foreach(x => println(x.mkString(" ")))


    matrixT(arrayA.length)(arrayB.length)
  }

  def matrixUpdate(matrix: Array[Array[Int]], row: Int, column: Int, valueToUpDate: Int): Unit = {
    matrix(row).update(column, valueToUpDate)
  }


}
