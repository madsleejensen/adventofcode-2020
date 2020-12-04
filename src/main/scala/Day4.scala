import scala.io.Source

object Day4 {
  def parse(lines: List[String]): List[Map[String, String]] = {
    val keyValueRegex = "^([^:]+):(.+)$".r

    case class PassportsAcc(acc: List[List[String]], cur: List[String])

    val grouped = lines.foldLeft(PassportsAcc(List(), List()))((acc, line) => line match {
      case "" => if (acc.cur.isEmpty) acc else PassportsAcc(acc.cur :: acc.acc, List()) // blank line
      case _ => PassportsAcc(acc.acc, line :: acc.cur)
    }) match {
      case PassportsAcc(acc, List()) => acc
      case PassportsAcc(acc, cur) => cur :: acc
    }

    grouped.map(
      group => group
        .flatMap(line => line.split(" ").toList)
        .map(kv => keyValueRegex findFirstMatchIn kv)
        .collect {
          case Some(x) => (x.group(1), x.group(2))
        }.toMap
    )
  }

  def partOne(passports: List[Map[String, String]]) = {
    val valid = passports.foldLeft(0)((acc, passport) => {
      if (passport.size == 8 || (passport.size == 7 && !passport.contains("cid")))
        acc + 1
      else
        acc
    })

    println(s"part-one: $valid valid passports")
  }

  /**
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
   */
  def validate(passport: Map[String, String]): Boolean = {
    if (passport.size < 7)
      return false

    if (passport.size == 7 && passport.contains("cid"))
      return false

    val byr = passport("byr").toInt
    if (byr < 1920 || byr > 2002)
      return false

    val iyr = passport("iyr").toInt
    if (iyr < 2010 || iyr > 2020)
      return false

    val eyr = passport("eyr").toInt
    if (eyr < 2020 || eyr > 2030)
      return false

    val hgt = passport("hgt")
    val hgtUnit = hgt.substring(hgt.length - 2)
    val hgtValue = hgt.substring(0, hgt.length - 2).toInt

    if (!hgt.matches("^[0-9]+(cm|in)$"))
      return false

    if (hgtUnit == "cm" && (hgtValue < 150 || hgtValue > 193))
      return false
    if (hgtUnit == "in" && (hgtValue < 59 || hgtValue > 76))
      return false

    val hcl = passport("hcl")
    if (!hcl.matches("^#[a-f0-9]{6}$"))
      return false

    val ecl = passport("ecl")
    val eclValid = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    if (!eclValid.contains(ecl))
      return false

    val pid = passport("pid")
    pid.matches("^[0-9]{9}$")
  }

  def partTwo(passports: List[Map[String, String]]) = {
    val valid = passports.count(i => validate(i))
    println(s"part-two: $valid valid passports")
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4/input.txt").getLines.toList
    val passports = parse(lines)

    partOne(passports)
    partTwo(passports)
  }
}
