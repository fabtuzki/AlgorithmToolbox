import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object PartitioningSouvenirs extends App {
  override def main(args: Array[String]): Unit = {


    val backtrackArray = partitionSouvenirs(108, Array(1, 2, 3, 4, 5, 5, 7, 7, 8, 10, 12, 19, 25))

    backtrackArray._2.foreach(x => println(x.mkString("|")))
    //    println(partitionSouvenirsUnwind(backtrackArray, 36, Array(1, 2, 3, 4, 5, 5, 7, 7, 8, 10, 12, 19, 25)))
  }

  //  def partitionSouvenirsUnwind(firstOut: (Int, Array[Array[ListBuffer[(Int, Int)]]]), weight: Int, goldBar: Array[Int]): Boolean = {
  //    val backtrackArray = firstOut._2
  //    if (firstOut._1 == weight) {
  //
  //      val elementList = new ListBuffer[List[Int]]
  //      //Purpose here is to find all possible way to get to 36 from the backtrack array:
  //      //How to find it in exclusive way?
  //
  //
  //    } else {
  //      false
  //    }
  //
  //
  //  }

  def partitionSouvenirs(weight: Int, goldBar: Array[Int]): (Int, Array[Array[ListBuffer[(Int, Int)]]]) = {
    //initiate the array of result:
    val matrixT = Array.ofDim[Int](goldBar.length + 1, weight + 1)
    //initiate the array to save the intermediate results:
    val matrixBacktrack = Array.ofDim[ListBuffer[(Int, Int)]](goldBar.length + 1, weight + 1)

    //initiate base case
    for (column <- 0 to weight) {
      matrixT(0).update(column, 0)
      matrixBacktrack(0).update(column, ListBuffer((-1, -1)))
    }

    for (row <- 0 to goldBar.length) {
      matrixT(row).update(0, 0)
      matrixBacktrack(row).update(0, ListBuffer((-1, -1)))

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
//        println("value maxValueWithoutMostRecentItem at row " + row + " and column " + column + " is " + maxValueWithoutMostRecentItem)
//        println("value maxValueAtWeightOfPreviousItem at row " + row + " and column " + column + " is " + maxValueAtWeightOfPreviousItem)
        if (finalValue == maxValueWithoutMostRecentItem && finalValue != maxValueAtWeightOfPreviousItem) {
          matrixBacktrack(row).update(column, ListBuffer((row - 1, column - goldBar(row - 1))))
        } else if (maxValueWithoutMostRecentItem == maxValueAtWeightOfPreviousItem) {
          matrixBacktrack(row).update(column, ListBuffer((row - 1, column - goldBar(row - 1))))
          matrixBacktrack(row)(column).append((row - 1, column))
        } else if (finalValue == maxValueAtWeightOfPreviousItem && finalValue != maxValueWithoutMostRecentItem) {
          matrixBacktrack(row).update(column, ListBuffer((row - 1, column)))
        }

        //        println("final value at row " + row + " and column " + column + " is " + finalValue)
        matrixT(row).update(column, finalValue)
      }
    }
    //
    println("print the matrix")
    matrixT.foreach(x => println(x.mkString(" ")))
    (matrixT(goldBar.length)(weight), matrixBacktrack)
  }

}
