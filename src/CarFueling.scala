import scala.util.control.Breaks

object CarFueling {
  def main(args: Array[String]): Unit = {

    println(carFueling(200, 250, Array(100,150)))
  }


  def carFueling(dist: Int, tank: Int, gasStation: Array[Int]): Int = {
    var stopCount = 0
    var currentPosition = 0
    val loop = new Breaks
    var nextStop = 0
    var currentStop = -1
    var leftDistance = dist
    loop.breakable {

      while (leftDistance >= tank) {
        for (i <- nextStop until gasStation.length) {
          nextStop = i
          if (currentPosition + tank <= gasStation(i) ) {
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
          currentPosition = gasStation(nextStop)
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
