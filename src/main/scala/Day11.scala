import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day11 {

  def partOne(grid: Seq[Seq[Char]]): Unit = {
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

    println(s"part-one: ${run(grid, 4, countNeighbors)}")
  }

  def partTwo(grid: Seq[Seq[Char]]) = {
    def countNeighbors(grid: Seq[Seq[Char]], x: Int, y: Int, char: Char) = {
      val vectors = List(
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1)
      )

      def check(vector: (Int, Int), x: Int, y: Int): Char = {
        val nX = x + vector._1
        val nY = y + vector._2

        if (nX < 0 || nX >= grid.head.length || nY < 0 || nY >= grid.length) {
          // out of bounds. found nothing
          return '.'
        }

        grid(nY)(nX) match {
          case '.' => check(vector, nX, nY)
          case c => c
        }
      }

      vectors.map(vector => check(vector, x, y)).count(_ == char)
    }

    println(s"part-two: ${run(grid, 5, countNeighbors)}")
  }

  def run(grid: Seq[Seq[Char]], occupied: Int, counter: (Seq[Seq[Char]], Int, Int, Char) => Int): Int = {
    val newGrid = grid.map(row => ListBuffer.from(row))
    var changes = 0

    for (y <- grid.indices; x <- grid(y).indices) {
      grid(y)(x) match {
        case 'L' if counter(grid, x, y, '#') == 0 =>
          newGrid(y)(x) = '#'
          changes += 1

        case '#' if counter(grid, x, y, '#') >= occupied =>
          newGrid(y)(x) = 'L'
          changes += 1

        case _ =>
      }
    }

    //      println(newGrid.map(row => row.mkString("")).mkString("\n"))
    //      println("changes", changes)

    changes match {
      case 0 => newGrid.flatten.count(_=='#')
      case _ => run(newGrid.map(_.toList), occupied, counter)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day11/input.txt").getLines.toList
    val grid = lines.map(_.toList)

    partOne(grid)
    partTwo(grid)
  }
}
