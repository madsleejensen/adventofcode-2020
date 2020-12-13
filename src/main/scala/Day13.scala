import scala.io.Source

object Day13 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13/input.txt").getLines.toList
    val List(estimate, busses, _*) = lines
    val from = estimate.toInt

    println(busses.split(',').filter(_ != "x").toList.map(i => {
      val bus = i.toInt
      val possible = math.ceil(from.toDouble / i.toDouble).toInt * bus
      val diff = possible - from

      (bus, diff)
    }).minBy(i => i._2) match {
      case (bus, diff) => bus * diff
    })
  }
}
