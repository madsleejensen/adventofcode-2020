import scala.io.Source

object Day3 {

  def count(lines: List[String], right: Int, down: Int): Int = {
    (down until lines.length by down).foldLeft(0)((acc, row) => {
      val line = lines(row)
      val index = (row/down * right) % line.length

      if (line(index) == '#')
        acc + 1
      else
        acc
    })
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3/input.txt").getLines.toList
    val partOne = count(lines, 3, 1)
    println(s"part-one: $partOne trees")

    val partTwo = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(i => count(lines, i._1, i._2).toLong).product
    println(s"part-two: $partTwo")
  }
}
