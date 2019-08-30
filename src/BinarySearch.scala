object BinarySearch {

  def main(args: Array[String]): Unit = {

    val searchArr = Array(1, 5, 8, 12, 13)

    val lookupArr = Array(8, 1, 23, 1, 11)


  }

  def binarySearch(searchArr: Array[Int], lookup: Int): Int = {
    val start = 0
    val end = searchArr.length
    val mid = math.ceil((start + end) / 2).toInt

    //compare:
    if (lookup == searchArr(mid)) {
      lookup
    } else if (lookup > searchArr(mid)) {
      val searchArrSlide = searchArr.slice(mid + 1, end)
      binarySearch(searchArrSlide, lookup)
    } else {
      val searchArrSlide = searchArr.slice(start, mid)
      binarySearch(searchArrSlide, lookup)
    }
  }


}
