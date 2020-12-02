import scala.io.Source

case class PasswordPolicyCheck(min: Int, max: Int, char: Char, password: String)

object Day2 {
  def partOne(matches: List[PasswordPolicyCheck]): Int = {
    matches.foldLeft(0)((acc, item) => {
      item match {
        case PasswordPolicyCheck(min, max, char, password) => {
          val count = password.count(_ == char)
          if (count >= min && count <= max) {
            acc + 1
          } else {
            acc
          }
        }
      }
    })
  }

  def partTwo(matches: List[PasswordPolicyCheck]): Int = {
    matches.foldLeft(0)((acc, item) => {
      item match {
        case PasswordPolicyCheck(min, max, char, password) => {
          var count = 0

          if (password(min - 1) == char)
            count += 1

          if (password(max - 1) == char)
            count += 1

          if (count == 1) {
            acc + 1
          } else {
            acc
          }
        }
      }
    })
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2/input.txt").getLines.toList
    val regex = "^([0-9]+)-([0-9]+) (.): (.+)$".r
    val matches = lines.map(line => regex findFirstMatchIn line).collect {
      case Some(m) => m.subgroups match {
        case List(min, max, search, password) => PasswordPolicyCheck(min.toInt, max.toInt, search(0), password)
      }
    }

    val partOneCount = partOne(matches)
    println(s"part-one: $partOneCount valid passwords")

    val partTwoCount = partTwo(matches)
    println(s"part-two: $partTwoCount valid passwords")
  }
}
