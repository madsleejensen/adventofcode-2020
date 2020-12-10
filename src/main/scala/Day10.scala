import scala.io.Source

object Day10 {
  def partOne(numbers: List[Int]): Unit = {
    def run(nums: List[Int], cur: Int, diffs: Map[Int, Int]): Map[Int, Int] = {
      nums match {
        case head :: tail => {
          val diff = head - cur
          val newDiffs = diffs.updated(diff, diffs(diff) + 1)
          run(tail, head, newDiffs)
        }
        case Nil => diffs
      }
    }

    val result = run(numbers.sorted :+ numbers.max + 3, 0, Map().withDefaultValue(0))

    println(s"part-one: $result ${result(1) * result(3)}")
  }

  def partTwo(numbers: List[Int]) = {
    val nums = numbers.sorted :+ numbers.max + 3
    val start: Map[Int, Long] = Map(0 -> 1L).withDefaultValue(0)
    val result = nums.foldLeft(start)((acc, num) => {
      val next = (1 to 3).map(i => {
        println(acc, num, num - i)
        acc(num - i)
      }).sum

      acc.updated(num, next)
    })

    println(s"part-two: ${result(nums.max)}")
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day10/input.txt").getLines.map(_.toInt).toList

    partOne(numbers)
    partTwo(numbers)
  }
}
