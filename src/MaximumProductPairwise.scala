import scala.io.Source
import scala.util.{Random => rd}

object MaximumProductPairwise {

  def main(args: Array[String]) = {
    val input = scala.io.Source.stdin.getLines().toArray
    var sortedArr = input(1).split(" ").map(_.toLong).sorted
    println(sortedArr(sortedArr.length - 1) * sortedArr(sortedArr.length - 2))


  }


}
