import scala.io.Source

object Day6 {
  def partOne(input: String): Int = {
    val groups = input.split("\n\n").toList
    groups.map(group => {
      val set = group.split("\n").flatMap(i => i.toList).toSet
      set.size
    }).sum
  }

  def partTwo(input: String): Int = {
    val groups = input.split("\n\n").toList
    groups.map(group => {
      val answers = group.split("\n").toList
      val chars = answers.mkString.toSet.map((c: Char) => c -> group.count(_ == c))
      val count = chars.count(x => x._2 == answers.length)

      count
    }).sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day6/input.txt").mkString.trim

    val partOneResult = partOne(input)
    println(s"part-one: $partOneResult")

    val partTwoResult = partTwo(input)
    println(s"part-two: $partTwoResult")
  }
}
