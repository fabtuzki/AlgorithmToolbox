object LeastCommonMultiple {
  def main(args: Array[String]): Unit = {
    println(LeastCommonMultiple(761457, 614573))
  }


  def LeastCommonMultiple(a: Int, b: Int): Long = {
    (a.toLong * b) / GreatestCommonDivisor.GreatestCommonDivisor(a, b)
  }

}
