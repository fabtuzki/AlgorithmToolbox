import scala.collection.mutable.ArrayBuffer

object NumberOfInversions {
  def main(args: Array[String]): Unit = {

    val inputArr = Array(2 ,3 ,9 ,2, 9)

    val outputArr = mergeSort(inputArr)
    outputArr._1.foreach(x => println(x))
    println("count inversion: " + outputArr._2)

  }

  def mergeSort(arr: Array[Int]): (Array[Int], Int) = {
    if (arr.length == 1) {
      return (arr, 0)
    }
    val middlePoint = math.floor((arr.length - 1) / 2) toInt
    val firstArr = mergeSort(arr.slice(0, middlePoint + 1))
    val secondArr = mergeSort(arr.slice(middlePoint + 1, arr.length))
    val arrayOut = Merge(firstArr, secondArr)
    return arrayOut
  }

  def Merge(firstArr: (Array[Int], Int), secondArr: (Array[Int], Int)): (Array[Int], Int) = {
    val outputArray = new ArrayBuffer[Int]
    var inversionCount = firstArr._2 + secondArr._2


    val firstArrBuffer = firstArr._1 toBuffer

    val secondArrBuffer = secondArr._1 toBuffer

    while (firstArrBuffer.nonEmpty && secondArrBuffer.nonEmpty) {

      val firstFirst = firstArrBuffer.head
      val firstSecond = secondArrBuffer.head
            println("first element of B " + firstFirst)
            println("first element of C " + firstSecond)
      if (firstFirst < firstSecond) {
        outputArray.append(firstFirst)
        firstArrBuffer.remove(0)
      } else if (firstFirst > firstSecond) {
        outputArray.append(firstSecond)
        secondArrBuffer.remove(0)
        inversionCount += firstArrBuffer.length
                println("first first > first second")
      } else {
        outputArray.append(firstFirst)
        firstArrBuffer.remove(0)
        println("first first = first second")
      }

    }

    if (firstArrBuffer.nonEmpty) {
            println("first array non empty")
      firstArrBuffer.foreach(x => {
        outputArray.append(x)
      })

    } else if (secondArrBuffer.nonEmpty) {
            println("second array non empty")
      secondArrBuffer.foreach(x => {
        outputArray.append(x)
      })
    }

    (outputArray.toArray, inversionCount)
  }

}
