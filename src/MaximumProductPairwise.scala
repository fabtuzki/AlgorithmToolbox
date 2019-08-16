import scala.util.{Random => rd}

object MaximumProductPairwise {

  def main(args: Array[String]) = {
    val input = Array.fill(10) {
      rd.nextInt(1000)
    }

    var sortedArr = input.sorted
    println(sortedArr(sortedArr.length - 1) * sortedArr(sortedArr.length - 2))


  }


}
