import scala.collection.mutable.ArrayBuffer

object BinarySearch extends App {

  override def main(args: Array[String]): Unit = {
        val input = scala.io.Source.stdin.getLines().toArray
        val searchArr = input(0).split(" ").drop(1).map(_.toInt)

        val lookupArr = input(1).split(" ").drop(1).map(_.toInt)

//    val searchArr = Array(1, 2, 3, 4, 5)
//    val lookupArr = Array(1, 2, 3, 4, 5)

    val out = new ArrayBuffer[Int]

    lookupArr.foreach(x => out.append(binarySearch(searchArr, x, 0, searchArr.length)))
    println(out.mkString(" "))

  }


  def binarySearch(searchArr: Array[Int], lookup: Int, start: Int, end: Int): Int = {
    val mid = math.ceil(start + (end - start) / 2).toInt
    if (start == end && start != lookup) {
      return -1
    }
    //compare:
    if (lookup == searchArr(mid)) {
      mid
    } else if (lookup > searchArr(mid)) {
      binarySearch(searchArr, lookup, mid + 1, end)
    } else {
      binarySearch(searchArr, lookup, start, mid)
    }
  }


}
