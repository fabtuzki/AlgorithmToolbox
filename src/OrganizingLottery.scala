import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object OrganizingLottery extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val pointArr = input(input.length - 1).split(" ").map(_.toLong)
    val inputSegment = input.dropRight(1).map(x => x.split(" ").map(_.toLong)).map(x => (x(0), x(1)))

    val out = organizingLottery(inputSegment, pointArr)

    println(out.mkString(" "))
  }


  def organizingLottery(segments: Array[(Long, Long)], points: Array[Long]): Array[Long] = {
    //This problem should be divided into 2 sub problems, one is sort by dq and then something similar to the problem
    //With open/close bracket of stack
    //First create the array:
    val arrayToCheck = new ArrayBuffer[(Long, String)]
    segments.foreach(x => {
      arrayToCheck.append((x._1, "open"))
      arrayToCheck.append((x._2, "close"))
    })

    points.foreach(x => arrayToCheck.append((x, "point")))

    val sortedArray = arrayToCheck.toArray
    sortedThePointRandomized(sortedArray, 0, arrayToCheck.length - 1)

    val outout = new ArrayBuffer[Long]
    val outHash = checkThePoint(sortedArray)
    points.foreach(x => outout.append(outHash.get(x).head))
    outout.toArray
  }


  def checkThePoint(arr: Array[(Long, String)]): mutable.HashMap[Long, Int] = {
    var checkOpenClose = 0
    val outputArr = new mutable.HashMap[Long, Int]
    for (i <- 0 until arr.length) {
      if (arr(i)._2 == "open") {
        checkOpenClose += 1
      }
      if (arr(i)._2 == "close") {
        checkOpenClose -= 1
      }
      if (arr(i)._2 == "point") {
        outputArr.put(arr(i)._1, checkOpenClose)
      }


    }
    outputArr
  }


  def sortedThePointRandomized(arr: Array[(Long, String)], left: Int, right: Int): Unit = {
    if (left >= right) {
      return
    }
    val randomSwap = Random.nextInt(right - left) + left
    swapModified(left, randomSwap, arr)
    val (splitPoint1, splitPoint2) = partition(arr, left, right)
    sortedThePointRandomized(arr, left, splitPoint1)

    sortedThePointRandomized(arr, splitPoint2, right)
  }

  def partition(arr: Array[(Long, String)], left: Int, right: Int): (Int, Int) = {

    val pivot = arr(left)._1
    var markPointLessThanPivot = left
    var markPointEqualPivot = left
    for (i <- left + 1 to right) {
      if (arr(i)._1 < pivot) {
        markPointLessThanPivot += 1
        swapModified(i, markPointLessThanPivot, arr)
        if (markPointEqualPivot > markPointLessThanPivot) {
          markPointEqualPivot += 1
          swapModified(i, markPointEqualPivot, arr)
        }
      } else if (arr(i)._1 == pivot) {
        if (markPointEqualPivot < markPointLessThanPivot) {
          markPointEqualPivot = markPointLessThanPivot + 1
          swapModified(i, markPointEqualPivot, arr)
        } else {
          markPointEqualPivot += 1
          swapModified(i, markPointEqualPivot, arr)
        }

      }

    }

    swapModified(markPointLessThanPivot, left, arr)

    if (markPointEqualPivot > markPointLessThanPivot) {
      (markPointLessThanPivot - 1, markPointEqualPivot + 1)

    } else {

      (markPointLessThanPivot - 1, markPointLessThanPivot + 1)


    }
  }

  def swapModified(a: Int, b: Int, arr: Array[(Long, String)]): Unit = {
    val arrA = arr(a)
    val arrB = arr(b)

    arr.update(a, arrB)
    arr.update(b, arrA)

  }

}
