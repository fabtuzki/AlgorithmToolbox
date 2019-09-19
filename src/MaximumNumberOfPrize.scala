import scala.collection.mutable.ArrayBuffer

object MaximumNumberOfPrize extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next()
    val out = maximumNumberOfPrize(input.toInt)
    println(out.length)
    println(out.mkString(" ")
    )
  }

  def maximumNumberOfPrize(k: Int): Array[Int] = {
    //Logic here to check is : the left over part of the segment has to be at least 2a + 3 , else return the whole left over.
    //if there is still space left to return, return the next value = previous + 1
    val output = new ArrayBuffer[Int]
    var leftCandy = k
    var potentialOutput = 0
    while (leftCandy > 0) {
      if (potentialOutput * 2 + 3 > leftCandy) {
        output.append(leftCandy)
        leftCandy = 0
      } else {
        output.append(potentialOutput + 1)
        leftCandy -= potentialOutput + 1
        potentialOutput = potentialOutput + 1
      }


    }
    output.toArray

  }


}
