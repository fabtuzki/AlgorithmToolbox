import scala.util.control.Breaks

object MaximumSalary {
  def main(args: Array[String]): Unit = {
    println(maximumSalary(Array(9897, 989)))
  }

  def maximumSalary(l: Array[Int]): Int = {
    val lSorted = l.sortWith(isGreaterThanOrEqualTo(_, _))
    lSorted.mkString.toInt

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
