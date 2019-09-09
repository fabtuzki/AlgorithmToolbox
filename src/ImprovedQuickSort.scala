import scala.util.Random

object ImprovedQuickSort {
  def main(args: Array[String]): Unit = {

    val inputArr = Array(1,0,2,45,20,20,23,21,25,26,46,62,1,5,2,2,0,0)


    improvedQuickSort(inputArr, 0, inputArr.length - 1)
    inputArr.foreach(x => println(x))

  }

  def improvedQuickSort(arr: Array[Int], left: Int, right: Int): Unit = {
    if (left >= right) {
      return
    }
    val randomSwap = Random.nextInt(right - left) + left
    swap(left, randomSwap, arr)
    val (splitPoint1, splitPoint2) = partition(arr, left, right)
    improvedQuickSort(arr, left, splitPoint1)

    improvedQuickSort(arr, splitPoint2, right)
  }

  def partition(arr: Array[Int], left: Int, right: Int): (Int, Int) = {

    val pivot = arr(left)
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
      }

      if (arr(i) == pivot) {
        if (markPointEqualPivot < markPointLessThanPivot) {
          markPointEqualPivot = markPointLessThanPivot + 1
          swap(i, markPointEqualPivot, arr)
        } else {
          markPointEqualPivot += 1
          swap(i, markPointEqualPivot, arr)
        }

      }

    }

    swap(markPointLessThanPivot, left, arr)

    if (markPointEqualPivot > markPointLessThanPivot) {
      (markPointLessThanPivot - 1, markPointEqualPivot + 1)

    } else {

      (markPointLessThanPivot - 1, markPointLessThanPivot + 1)


    }
  }

  def swap(a: Int, b: Int, arr: Array[Int]): Unit = {
    val arrA = arr(a)
    val arrB = arr(b)

    arr.update(a, arrB)
    arr.update(b, arrA)

  }
}
