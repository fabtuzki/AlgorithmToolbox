object MaximumSalary extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val numberList = input(1).split(" ").map(_.toInt)

    println(maximumSalary(numberList))
  }

  def maximumSalary(l: Array[Int]): Double = {
    val lSorted = l.sortWith(isGreaterThanOrEqualTo(_, _))
    lSorted.mkString.toDouble

  }


  def isGreaterThanOrEqualTo(a: Int, b: Int): Boolean = {
    val firstCom = (a.toString + b.toString).toInt
    val secondCom = (b.toString + a.toString).toInt
    if (firstCom != secondCom) {
      if (firstCom > secondCom) {
        true
      } else {
        false
      }
    } else {
      true
    }
  }
}
