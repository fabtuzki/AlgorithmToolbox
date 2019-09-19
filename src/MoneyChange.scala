
object MoneyChange extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().next()

    println(MoneyChange(input.toInt))

  }


  def MoneyChange(change: Int): Int = {
    var listOfCoin = 0
    var changeLeft = change
    while (changeLeft >= 10) {
      changeLeft -= 10
      listOfCoin += 1
    }
    while (changeLeft >= 5) {
      changeLeft -= 5
      listOfCoin += 1
    }
    while (changeLeft >= 1) {
      changeLeft -= 1
      listOfCoin += 1
    }
    listOfCoin

  }
}
