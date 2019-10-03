import scala.util.control.Breaks

object CarFueling extends App {
  override def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    val dist = input(0).toLong
    val tank = input(1).toLong
    val stops = input(3).split(" ").map(_.toLong)
    println(carFueling(dist, tank, stops))

    //    println(carFueling(500, 200, Array(100L, 200L, 300L, 400L)))

  }


  def carFueling(dist: Long, tank: Long, gasStation: Array[Long]): Long = {
    var stopCount = 0L
    var currentPosition = 0L
    val loop = new Breaks
    var nextStop = 0L
    var currentStop = -1L
    var leftDistance = dist
    loop.breakable {

      while (leftDistance >= tank) {
        for (i <- nextStop.toInt until gasStation.length) {
          nextStop = i
          if (currentPosition + tank < gasStation(i)) {
            if (i - 1 != currentStop) {
              currentPosition = gasStation(i - 1)
              stopCount += 1
              currentStop = i - 1
              leftDistance -= currentPosition
            } else {
              loop.break()
            }
          }

        }
        if (nextStop == gasStation.length - 1) {
          currentPosition = gasStation(nextStop.toInt)
          stopCount += 1
          loop.break()
        }
      }

    }
    if (currentPosition + tank >= dist) {
      stopCount
    } else {
      -1
    }
  }
}
