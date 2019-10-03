import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object CollectSignature extends App {
  override def main(args: Array[String]): Unit = {

    //    val input = scala.io.Source.stdin.getLines().toArray.drop(1).map(_.split(" ")).map(x => (x(0).toInt, x(1).toInt))
    //    val out = collectSignature(input)
    val out = collectSignature(Array((41, 42),
      (52, 52),
      (63, 63),
      (80, 82),
      (78, 79),
      (35, 35),
      (22, 23),
      (31, 32),
      (44, 45),
      (81, 82),
      (36, 38),
      (10, 12),
      (1, 1),
      (23, 23),
      (32, 33),
      (87, 88),
      (55, 56),
      (69, 71),
      (89, 91),
      (93, 93),
      (38, 40),
      (33, 34),
      (14, 16),
      (57, 59),
      (70, 72),
      (36, 36),
      (29, 29),
      (73, 74),
      (66, 68),
      (36, 38),
      (1, 3),
      (49, 50),
      (68, 70),
      (26, 28),
      (30, 30),
      (1, 2),
      (64, 65),
      (57, 58),
      (58, 58),
      (51, 53),
      (41, 41),
      (17, 18),
      (45, 46),
      (4, 4),
      (0, 1),
      (65, 67),
      (92, 93),
      (84, 85),
      (75, 77),
      (39, 41),
      (15, 15),
      (29, 31),
      (83, 84),
      (12, 14),
      (91, 93),
      (83, 84),
      (81, 81),
      (3, 4),
      (66, 67),
      (8, 8),
      (17, 19),
      (86, 87),
      (44, 44),
      (34, 34),
      (74, 74),
      (94, 95),
      (79, 81),
      (29, 29),
      (60, 61),
      (58, 59),
      (62, 62),
      (54, 56),
      (58, 58),
      (79, 79),
      (89, 91),
      (40, 42),
      (2, 4),
      (12, 14),
      (5, 5),
      (28, 28),
      (35, 36),
      (7, 8),
      (82, 84),
      (49, 51),
      (2, 4),
      (57, 59),
      (25, 27),
      (52, 53),
      (48, 49),
      (9, 9),
      (10, 10),
      (78, 78),
      (26, 26),
      (83, 84),
      (22, 24),
      (86, 87),
      (52, 54),
      (49, 51),
      (63, 64),
      (54, 54)))
    println(out.length)
    println(out.mkString(" "))

  }

  def collectSignature(timeSegment: Array[(Int, Int)]): Array[Int] = {
    //sort segment by min value of the segments
    val timeSegmentSorted = timeSegment.sortBy(x => x._1)
    println("array segment :  " + timeSegmentSorted.mkString(" "))

    //add the segment into a bucket until cannot find a middle point, then remove those segment, add point and repeat
    val listOfPoint = new ListBuffer[Int]

    var currentSegment = 0

    var minSegment = timeSegmentSorted(currentSegment)._1
    var maxSegment = timeSegmentSorted(currentSegment)._2

    while (currentSegment < timeSegmentSorted.length - 1) {
      val loop = new Breaks
      loop.breakable {
        for (i <- currentSegment + 1 until timeSegmentSorted.length) {
          println("start loop with i value " + (currentSegment + 1))
          //nếu vị trí current segment + i là vị trí cuối cùng của chuỗi segment:
          if (currentSegment + i == timeSegmentSorted.length - 1) {
            println("current segment + i is equal to length of array segment")
            currentSegment = timeSegmentSorted.length - 1 //update giá trị current segment thành giá trị cuối cùng của array segment
            minSegment = timeSegmentSorted(currentSegment)._1 //giá trị min segment = giá trị ....
            listOfPoint.append(minSegment) //danh sách output gắn thêm minsegment
            loop.break() // break loop tại đây
          } else if (currentSegment + i < timeSegmentSorted.length - 1 &&
            timeSegmentSorted(currentSegment + i)._1 > maxSegment) {
            println("case non over lap of previous segments and current segment at value of i = " + i + " timeSegmentSorted(currentSegment + i)._1 " + timeSegmentSorted(currentSegment + i)._1 + " max segment  " + maxSegment)
            //Case vẫn đang loop through array segment giá trị min của array segment tại current segment + i lớn hơn max segment
            //Tức là không có over lap giữa toàn bộ segment cũ và segment mới
            listOfPoint.append(minSegment) //gán giá trị min segment gần nhất
            currentSegment = currentSegment + i //update giá trị current segment
            minSegment = timeSegmentSorted(currentSegment)._1 //update giá trị min segment
            maxSegment = timeSegmentSorted(currentSegment)._2 //update giá trị max segment
            loop.break() //break loop này để start loop mới
          } else if (currentSegment + i < timeSegmentSorted.length - 1 &&
            timeSegmentSorted(currentSegment + i)._1 <= maxSegment) {
            //Case min segment của giá trị currentSegment + i vẫn nhỏ hơn max segment hiện tại
            println("case exists over lap of previous segments and current segment at value of i = " + i + " timeSegmentSorted(currentSegment + i)._1 " + timeSegmentSorted(currentSegment + i)._1 + " max segment  " + maxSegment)
            println("min segmnent is " + minSegment)
            minSegment = timeSegmentSorted(currentSegment + i)._1 //update min segment thành giá trị min tiếp theo
            println("min segmnent updated is " + minSegment)
            if (maxSegment > timeSegmentSorted(currentSegment + i)._2) {
              maxSegment = timeSegmentSorted(currentSegment + i)._2 //nếu max segment hiện tại lớn hơn max segment của current segment + i thì update
              println("max segmnent updated is " + maxSegment)
            }
          }
        }
      }


    }

    listOfPoint.toArray
  }
}
