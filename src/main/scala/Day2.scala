import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2/input.txt").getLines.toList
    val regex = "^([0-9]+)-([0-9]+) (.): (.+)$".r

    val valid = lines.foldLeft(0)((acc, line) => {
      regex findFirstMatchIn line match {
        case Some(result) =>
          result.subgroups match {
            case List(min, max, search, password) => {
              val char = search(0)
              val count = password.count(_ == char)

              if (count >= min.toInt && count <= max.toInt) {
                acc + 1
              } else {
                acc
              }
            }
          }
        case None => acc
      }
    })

    println(s"found $valid valid passwords")
  }
}
