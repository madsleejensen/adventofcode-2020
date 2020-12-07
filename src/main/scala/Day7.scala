import scala.io.Source

case class Bag(name: String, quantity: Int)

object Day7 {
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

    println(colors.keys.count(p => doSearch(colors(p))))
  }
}
