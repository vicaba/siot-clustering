package test

object Main {

  def main(args: Array[String]): Unit = {
    LineChartSample.start(
      data = List[(Number, Number)](
      (1, 23),
      (2, 27),
      (3, 15),
      (4, 24),
      (5, 34),
      (6, 36),
      (7, 22),
      (8, 45)
    ))
  }

  def approximateMethod(): Unit = {

  }

}
