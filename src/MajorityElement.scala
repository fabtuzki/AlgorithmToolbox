object MajorityElement {

  def main(args: Array[String]): Unit = {

  }


  def majorityElement(arr: Array[Int]): Array[Int] = {
    if (arr.length == 1) {
      return arr
    }

    val m = math.floor((arr.length - 1) / 2).toInt
    val B = arr.slice(0, m)
    val C = arr.slice(m, arr.length)
    val Out = majorityCount(B, C)
    return Out
  }


  def majorityCount(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {
    null
  }
}
