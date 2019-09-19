import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object CollectSignature extends App {
  override def main(args: Array[String]): Unit = {

    //    val input = scala.io.Source.stdin.getLines().toArray.drop(1).map(_.split(" ")).map(x => (x(0).toInt, x(1).toInt))

    val out = collectSignature(Array((4, 7),
      (1, 3),
      (2, 5),
      (5, 6)))
    println(out.length)
    println(out)

  }

  def collectSignature(timeSegment: Array[(Int, Int)]): String = {
    //sort segment by min value of the segments
    val timeSegmentSorted = timeSegment.sortBy(x => x._1)

    //add the segment into a bucket until cannot find a middle point, then remove those segment, add point and repeat
    val listOfPoint = new ListBuffer[Int]

    var currentSegment = 0

    var minSegment = timeSegmentSorted(currentSegment)._1
    var maxSegment = timeSegmentSorted(currentSegment)._2

    while (currentSegment < timeSegmentSorted.length - 1) {
      val loop = new Breaks
      loop.breakable {
        for (i <- currentSegment + 1 until timeSegmentSorted.length) {
          if (i == timeSegmentSorted.length - 1) {
            currentSegment = timeSegmentSorted.length - 1
            minSegment = timeSegmentSorted(currentSegment)._1
            listOfPoint.append(minSegment)
            loop.break()
          }
          //Case 2 segment không có điểm chung
          if (timeSegmentSorted(currentSegment + i)._1 > maxSegment) {
            listOfPoint.append(minSegment)
            currentSegment = currentSegment + i
            minSegment = timeSegmentSorted(currentSegment)._1
            maxSegment = timeSegmentSorted(currentSegment)._2
            loop.break()
          } else if (timeSegmentSorted(currentSegment + i)._1 <= maxSegment) {
            minSegment = timeSegmentSorted(currentSegment + i)._1
            if (maxSegment > timeSegmentSorted(currentSegment + i)._2) {
              maxSegment = timeSegmentSorted(currentSegment + i)._2
            }

          }
        }
      }


    }

    listOfPoint.mkString(" ")
  }
}
