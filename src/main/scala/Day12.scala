import scala.io.Source

object Day12 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day12/input.txt").getLines

    val Command = "([A-Z])([0-9]+)".r

    val map = Map(
      'N' -> 0,
      'E' -> 90,
      'S' -> 180,
      'W' -> 270,
    )

    def add(n: Int, vec: (Int, Int, Int)) = {
      val (x, y, deg) = vec
      deg match {
        // east
        case 90 => (x + n.toInt, y, deg)
        // south
        case 180 => (x, y - n.toInt, deg)
        // west
        case 270 => (x - n.toInt, y, deg)
        // north
        case _ => (x, y + n.toInt, deg)
      }
    }

    println(lines.foldLeft((0, 0, 90))((acc, line) => {
      println(acc)
      val (x, y, deg) = acc

      line match {
        case Command("F", n) => add(n.toInt, (x, y, deg))
        case Command("R", n) => (x, y, (360 + (deg + n.toInt)) % 360)
        case Command("L", n) => (x, y, (360 + (deg - n.toInt)) % 360)
        case Command(direction, n) => {
          val (nX, nY, _) = add(n.toInt, (x, y, map(direction(0))))
          (nX, nY, deg)
        }
        case _ => acc
      }
    }) match {
      case (x, y, _) => math.abs(x) + math.abs(y)
    })
  }
}
