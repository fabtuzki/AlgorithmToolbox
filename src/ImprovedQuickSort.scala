import scala.util.Random

object ImprovedQuickSort extends App {
  override def main(args: Array[String]): Unit = {

    val input = scala.io.Source.stdin.getLines().toArray
    val inputArr = input(1).split(" ").map(_.toLong)

    //    val inputArr = Array(1000000000L, 1, 1000000000, 1000000000, 1000000000, 1000000000, 1000000000, 1000000000, 1000000000, 1, 1, 1, 1, 1, 1, 1, 1000000000, 1000000000, 1000, 1000000000, 1000000000, 1000, 1000, 1000000000, 1000000000, 1000000000, 1000000000, 1000000000)
    improvedQuickSort(inputArr, 0, inputArr.length - 1)
    println(inputArr.mkString(" "))
  }

  def improvedQuickSort(arr: Array[Long], left: Int, right: Int): Unit = {
    if (left >= right) {
      return
    }
    val randomSwap = Random.nextInt(right - left) + left
    //    println("randomswap is " + randomSwap)
    //    println("current left  " + left)
    //    println("current right " + right)
    //    println("array before swap  " + arr.mkString(" "))
    swap(left, randomSwap, arr)
    //    println("array after swap  " + arr.mkString(" "))
    val (splitPoint1, splitPoint2) = partition(arr, left, right)
    //    println("current splitting point 1 is " + splitPoint1 + " current splittling point 2 is " + splitPoint2)
    improvedQuickSort(arr, left, splitPoint1 - 1)
    improvedQuickSort(arr, splitPoint2 + 1, right)
  }

  def partition(arr: Array[Long], left: Int, right: Int): (Int, Int) = {

    val pivot = arr(left)
    //    println("pivot is " + pivot)
    var markPointLessThanPivot = left
    var markPointEqualPivot = left
    for (i <- left + 1 to right) {
      if (arr(i) < pivot) {
        markPointLessThanPivot += 1
        swap(i, markPointLessThanPivot, arr)
        if (markPointEqualPivot > markPointLessThanPivot) {
          markPointEqualPivot += 1
          swap(i, markPointEqualPivot, arr)
        }
      } else if (arr(i) == pivot) {
        if (markPointEqualPivot < markPointLessThanPivot) {
          markPointEqualPivot = markPointLessThanPivot + 1
          swap(i, markPointEqualPivot, arr)
        } else {
          markPointEqualPivot += 1
          swap(i, markPointEqualPivot, arr)

        }

      }
      //      println("array after check " + i + " " + arr.mkString(" "))
      //      println("k is " + markPointEqualPivot)
      //      println("j is " + markPointLessThanPivot)

    }

    swap(markPointLessThanPivot, left, arr)

    if (markPointEqualPivot > markPointLessThanPivot) {
      (markPointLessThanPivot, markPointEqualPivot)

    } else {

      (markPointLessThanPivot, markPointLessThanPivot)


    }
  }

  def swap(a: Int, b: Int, arr: Array[Long]): Unit = {
    val arrA = arr(a)
    val arrB = arr(b)

    arr.update(a, arrB)
    arr.update(b, arrA)

  }


}
