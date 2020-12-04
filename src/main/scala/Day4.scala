import scala.io.Source


case class PassportsAcc(acc: List[List[String]], cur: List[String])

object Day4 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4/input.txt").getLines

    val keyValueRegex = "^([^:]+):(.+)$".r

    val grouped = lines.foldLeft(PassportsAcc(List(), List()))((acc, line) => line match {
      case "" => if (acc.cur.isEmpty) acc else PassportsAcc(acc.cur :: acc.acc, List()) // blank line
      case _ => PassportsAcc(acc.acc, line :: acc.cur)
    }) match {
      case PassportsAcc(acc, List()) => acc
      case PassportsAcc(acc, cur) => cur :: acc
    }

    val passports = grouped.map(
      group => group
        .flatMap(line => line.split(" ").toList)
        .map(kv => keyValueRegex findFirstMatchIn kv)
        .collect {
          case Some(x) => (x.group(1), x.group(2))
        }.toMap
    )

    println(passports.foldLeft(0)((acc, passport) => {
      if (passport.size == 8 || (passport.size == 7 && !passport.contains("cid")))
        acc + 1
      else
        acc
    }))

  }
}
