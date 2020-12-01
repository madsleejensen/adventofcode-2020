import scala.io.Source

object Day1 {
  def find(search: Int, combinationCount: Int, lines: List[String]): Option[Int] = {
    val result = lines.map(i => i.toInt)
      .combinations(combinationCount)
      .find(combination => combination.sum == search)

    result.map(i => i.product)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1/input.txt").getLines.toList

    // first part
    val first = find(2020, 2, lines)
    println(s"first => ${first}")

    // second part
    val second = find(2020, 3, lines)
    println(s"second => ${second}")
  }
}
