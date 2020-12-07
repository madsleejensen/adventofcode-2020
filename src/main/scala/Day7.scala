import scala.io.Source

case class Bag(name: String, quantity: Int)

object Day7 {
  def partOne(colors: Map[String, List[Bag]]): Int = {
    def doSearch(list: List[Bag]): Boolean = {
      list match {
        case head :: tail =>
          if (head.name == "shiny gold")
            return true

          val found = colors.get(head.name) match {
            case Some(x) => doSearch(x)
            case None => false
          }

          found || doSearch(tail)
        case _ => false
      }
    }

    colors.keys.count(p => doSearch(colors(p)))
  }

  def partTwo(bags: Map[String, List[Bag]]) = {
    val innerBags = bags("shiny gold")

    def doCount(list: List[Bag]): Int = {
      list match {
        case head :: tail => {
          val curr = bags.get(head.name) match {
            case Some(x) => {
              val sub = doCount(x)
              head.quantity + head.quantity * sub
            }
            case None => head.quantity
          }

          println(s"$head -> $curr")

          curr + doCount(tail)
        }
        case Nil => 0
      }
    }

    doCount(innerBags)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day7/input.txt").getLines
    val bagPattern = "^([0-9]+) (.+?) bag.*".r
    val linePattern = "^(.+?) bags contain (.+)$".r

    val colors: Map[String, List[Bag]] = lines.map {
      case linePattern(x, bags) => x -> bags
        .split(',')
        .toList
        .map(i => {
          i.trim match {
            case bagPattern(quantity, name) => Bag(name, quantity.toInt)
            case _ => Nil
          }
        })
        .collect {
          case bag: Bag => bag
        }
    }.toMap

    val partOneResult = partOne(colors)
    println(s"part-one: $partOneResult bags")

    val partTwoResult = partTwo(colors)
    println(s"part-two: $partTwoResult bags")

  }
}
