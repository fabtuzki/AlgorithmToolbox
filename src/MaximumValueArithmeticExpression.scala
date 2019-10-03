object MaximumValueArithmeticExpression extends App {
  override def main(args: Array[String]): Unit = {
    val expression = scala.io.Source.stdin.getLines().next()
//        val expression = "9*9*9*9*9*9*9*9*9*9*9*9*9*9"
    println(maximumValueArithmeticExpression(expression))

  }


  def maximumValueArithmeticExpression(expression: String): Long = {
    val expressionArr = expression.toCharArray

    val digitArr = expressionArr.zipWithIndex.filter(x => {
      modulo(x._2, 2) == 0
    }).map(_._1.toString.toInt)
    //    println("digit array " + digitArr.mkString(" "))
    val operationArr = expressionArr.zipWithIndex.filter(x => {
      modulo(x._2, 2) == 1
    }).map(_._1)

    //    println("operation array " + operationArr.mkString(" "))
    //initiate the array of min and max
    val minArray = Array.ofDim[Long](digitArr.length, digitArr.length)
    val maxArray = Array.ofDim[Long](digitArr.length, digitArr.length)

    //fill on base case :
    for (i <- 0 until digitArr.length) {
      minArray(i).update(i, digitArr(i))
      maxArray(i).update(i, digitArr(i))
    }

    //loop through the digit to fill min and max array:
    for (diff <- 1 until digitArr.length) {
      var column = diff
      var row = 0

      while (column <= digitArr.length - 1) {


        //set out the min and the max value
        var min = 1000000000000000000L
        var max = -1000000000000000000L

        //loop again from row to column
        for (i <- row until column) {
          //min min
          val minmin = operate(minArray(row)(i),
            minArray(i + 1)(column),
            operationArr(i))
          //min max
          val minmax = operate(minArray(row)(i), maxArray(i + 1)(column), operationArr(i))
          //max min
          val maxmin = operate(maxArray(row)(i), minArray(i + 1)(column), operationArr(i))
          //max max
          val maxmax = operate(maxArray(row)(i), maxArray(i + 1)(column), operationArr(i))
          val finalMin = Array(minmin, minmax, maxmin, maxmax).min
          val finalMax = Array(minmin, minmax, maxmin, maxmax).max

          if (finalMin < min) {
            min = finalMin
          }
          if (finalMax > max) {
            max = finalMax
          }

        }

        minArray(row).update(column, min)
        maxArray(row).update(column, max)

        row += 1
        column += 1


      }


    }

//    println("min array is ")
//    minArray.foreach(x => println(x.mkString(" ")))
//    println("max array is ")
//    maxArray.foreach(x => println(x.mkString(" ")))

    maxArray(0)(digitArr.length - 1)


  }

  def operate(x: Long, y: Long, op: Char): Long = op match {
    case '+' => (x + y)
    case '-' => (x - y)
    case '*' => (x * y)
  }

  def modulo(dividend: Double, divisor: Long): Int = {
    if (dividend < divisor) {
      dividend.toInt
    } else {
      var remainder = 0D
      val estimatedQ = math.floor(dividend / divisor).toLong
      remainder = dividend - estimatedQ * divisor
      while (remainder >= divisor) {
        remainder = remainder - divisor
      }

      remainder.toInt

    }
  }

}
