import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3/input.txt").getLines.toList
    val rows = lines.length

    println((1 until rows).foldLeft(0)((acc, row) => {
      val line = lines(row)
      val index = (row * 3) % line.length
      if (line(index) == '#')
        acc + 1
      else
        acc
    }))
  }
}
