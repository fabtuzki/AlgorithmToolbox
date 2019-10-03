import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object OrganizingLottery extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val pointArr = input(input.length - 1).split(" ").map(_.toLong)
    val inputSegment = input.dropRight(1).map(x => x.split(" ").map(_.toLong)).map(x => (x(0), x(1)))

    val out = organizingLottery(inputSegment, pointArr)
    /*    val out = organizingLottery(Array((6, 21),
          (6, 12),
          (8, 10),
          (0, 2),
          (0, 4),
          (-6, -2),
          (-4, -2)), Array(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))*/
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

    //    println("sorted array is " + sortedArray.mkString(" "))

    val outout = Array.ofDim[Long](points.length)
    val outHash = checkThePoint(sortedArray)

    //    println("hashed output " + outHash.mkString(" , "))

    for (i <- 0 until points.length) {
      //      println("point checking " + points(i) + " value of hash is " + outHash.get(points(i)).head)
      outout.update(i, outHash.get(points(i)).head)
    }
    outout
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
    //    println("randomswap is " + randomSwap)
    //    println("current left  " + left)
    //    println("current right " + right)
    //    println("array before swap  " + arr.mkString(" "))
    swapModified(left, randomSwap, arr)
    //    println("array after swap  " + arr.mkString(" "))
    val (splitPoint1, splitPoint2) = partition(arr, left, right)
    //    println("splitting point 1 = " + splitPoint1 + " at value " + arr(splitPoint1) + "\n splitting point 2 = " + splitPoint2 + " at value " + arr(splitPoint2))
    //    println("full array is : " + arr.mkString(" "))
    if (splitPoint2 - splitPoint1 > 0) {
      sortTheMiddle(arr, (splitPoint1, splitPoint2))
    }
    sortedThePointRandomized(arr, left, splitPoint1 - 1)
    sortedThePointRandomized(arr, splitPoint2 + 1, right)
  }


  def sortTheMiddle(sortedArray: Array[(Long, String)], splitPoints: (Int, Int)): Unit = {
    //Thứ tự sort là open, points, close
    //Sử dụng count sort cho dễ:
    val subArr = sortedArray.slice(splitPoints._1, splitPoints._2 + 1)
    val hashM = new mutable.HashMap[String, Int]
    val value = sortedArray(splitPoints._1)._1
    subArr.foreach(x => {
      //      println("value check " + x)
      if (hashM.getOrElse(x._2, 0) == 0) {
        hashM.put(x._2, 1)
      } else {
        hashM.put(x._2, hashM.get(x._2).head + 1)
      }
    }
    )

    //    println("output the hashmap for value " + value + " : " + hashM.mkString(" "))
    val countOpen = hashM.getOrElse("open", 0)
    val countPoint = hashM.getOrElse("point", 0) + countOpen

    val countClose = hashM.getOrElse("close", 0) + countPoint

    for (i <- 0 until subArr.length) {
      if (countOpen > 0 && i <= countOpen - 1) {
        sortedArray.update(splitPoints._1 + i, (value, "open"))
      }
      if (countPoint > 0 && i <= countPoint - 1 && i > countOpen - 1) {
        sortedArray.update(splitPoints._1 + i, (value, "point"))
      }
      if (countClose > 0 && i <= countClose - 1 && i > countPoint - 1) {
        sortedArray.update(splitPoints._1 + i, (value, "close"))
      }

    }

    //    //check correct
    //    if (i < (splitPoints._2 - splitPoints._1 + 1)) throw new IllegalStateException("hasn't finished all value in sub array")
  }

  def partition(arr: Array[(Long, String)], left: Int, right: Int): (Int, Int) = {
    //vị trí của J là ở vị trí cuối cùng của array < pivot
    //vị trí của K là ở vị trí cuối cùng của array = pivot
    val pivot = arr(left)._1
    //        println("pivot is " + arr(left))
    var markPointLessThanPivot = left
    var markPointEqualPivot = left
    for (i <- left + 1 to right) {
      //            println("value comparing : " + arr(i))
      //Case 1: Nếu value ở i < pivot thì:
      if (arr(i)._1 < pivot) {
        //        println("case 1, value arr i < pivot")
        //Markpoint J +=1, swap vị trí i và J
        markPointLessThanPivot += 1
        swapModified(i, markPointLessThanPivot, arr)
        //Nếu markpoint K > mark J thì K += 1 và swap vị trí i và K
        if (markPointEqualPivot >= markPointLessThanPivot) {
          markPointEqualPivot += 1
          swapModified(i, markPointEqualPivot, arr)
        }
      } else if (arr(i)._1 == pivot) {
        //        println("case 2, value arr i = pivot")
        //Case 2: Nếu value ở i mà = pivot:
        /*Trong case này bây giờ phải thêm trường hợp so sánh giá trị thứ 2 để xem swap ntn.
        * Ở đây nên gọi 1 hàm riêng hay viết thẳng vào nhỉ?
        * Không đúng, đây là bóc ra đoạn có hệ số thứ 1 bằng nhau, nên về cuối cùng 2 biến mark point mới là chuẩn nhất.
        * Như vậy việc so sánh sort lại phải được thực hiện sau cùng, sau khi chốt 2 markpoint */

        //Nếu markpoint K = left thì K = J + 1 , swap i với K
        //Nếu markpoint K > left và J = left thì tiếp tục add K + 1, swap i với k, và giữ nguyên J
        //Nếu markpoint K > left và J > left , và K > J thì K + 1, swap i với K ,
        //Nếu markpoint K > left và J > left nhưng K < J thì throw exception error
        if (markPointEqualPivot == left) {
          markPointEqualPivot = markPointLessThanPivot + 1
          swapModified(i, markPointEqualPivot, arr)
        } else if (markPointEqualPivot > left && markPointLessThanPivot == left) {
          markPointEqualPivot += 1
          swapModified(i, markPointEqualPivot, arr)
        } else if (markPointEqualPivot > left && markPointLessThanPivot > left && markPointEqualPivot > markPointLessThanPivot) {
          markPointEqualPivot += 1
          swapModified(i, markPointEqualPivot, arr)
        } else if (markPointEqualPivot > left && markPointLessThanPivot > left && markPointEqualPivot <= markPointLessThanPivot) {
          //          println("current i is " + i)
          //          println("current array " + arr.mkString(" "))
          throw new IllegalArgumentException("error condition with markpoint K " + markPointEqualPivot + " and markpoint J " + markPointLessThanPivot)
        }


      }

    }

    swapModified(markPointLessThanPivot, left, arr)

    if (markPointEqualPivot > markPointLessThanPivot) {
      (markPointLessThanPivot, markPointEqualPivot)

    } else {

      (markPointLessThanPivot, markPointLessThanPivot)


    }
  }


  def swapModified(a: Int, b: Int, arr: Array[(Long, String)]): Unit = {
    val arrA = arr(a)
    val arrB = arr(b)

    arr.update(a, arrB)
    arr.update(b, arrA)

  }


}
