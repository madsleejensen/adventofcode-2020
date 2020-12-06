import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day6/input.txt").mkString.trim

    val groups = input.split("\n\n").toList
    println(groups.map(group => {
      val set = group.split("\n").flatMap(i => i.toList).toSet
      set.size
    }).sum)


  }
}
