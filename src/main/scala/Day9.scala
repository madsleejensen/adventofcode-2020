import scala.annotation.tailrec
import scala.io.Source

object Day9 {
  def partOne(numbers: List[Long]): Long = {
    val preamble = numbers.take(25)
    val rest = numbers.drop(25)

    def run(preamble: List[Long], rest: List[Long]): Long = {
      rest match {
        case head :: tail =>
          if (preamble.combinations(2).map(_.sum).contains(head)) {
            run(preamble.drop(1) :+ head, tail)
          } else {
            head
          }
      }
    }

    run(preamble, rest)
  }

  @tailrec
  def findWeakness(list: List[Long], invalid: Long): Long = {
    @tailrec
    def findContiguousNumbers(nums: List[Long], acc: List[Long]): Option[Long] = nums match {
      case head :: tail =>
        val newAcc = head :: acc
        if (newAcc.sum == invalid) {
          Some(newAcc.max + newAcc.min)
        } else if (newAcc.sum > invalid) {
          None
        }
        else
          findContiguousNumbers(tail, newAcc)
    }

    list match {
      case head :: tail =>
        findContiguousNumbers(tail, List(head)) match {
          case Some(weakness) => weakness
          case None => findWeakness(tail, invalid)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day9/input.txt").getLines.map(_.toLong).toList

    val invalid = partOne(numbers)
    println(s"part-one: $invalid")

    val weakness = findWeakness(numbers, invalid)
    println(s"part-two: $weakness")
  }
}
