import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day11 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day11/input.txt").getLines.toList
    val grid = lines.map(_.toList)

    def countNeighbors(grid: Seq[Seq[Char]], x: Int, y: Int, char: Char) = {
      val chars = for {
        ny <- y - 1 to y + 1
        nx <- x - 1 to x + 1
        if ny >= 0 && ny < grid.length
        if nx >= 0 && nx < grid(ny).length
        if !(ny == y && nx == x)
      } yield grid(ny)(nx)

      chars.count(_ == char)
    }

    def run(grid: Seq[Seq[Char]]): Int = {
      val newGrid = grid.map(row => ListBuffer.from(row))
      var changes = 0

      for (y <- grid.indices; x <- grid(y).indices) {
        grid(y)(x) match {
          case 'L' if countNeighbors(grid, x, y, '#') == 0 =>
            newGrid(y)(x) = '#'
            changes += 1

          case '#' if countNeighbors(grid, x, y, '#') >= 4 =>
            newGrid(y)(x) = 'L'
            changes += 1

          case _ =>
        }
      }

//      println(newGrid.map(row => row.mkString("")).mkString("\n"))
//      println("changes", changes)

      changes match {
        case 0 => newGrid.flatten.count(_=='#')
        case _ => run(newGrid.map(_.toList))
      }
    }

    println(run(grid))
  }
}
