import scala.collection.mutable.ArrayBuffer

object MoneyChange {
  def main(args: Array[String]): Unit = {
    println(MoneyChange(28))

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
