import scala.io.Source


sealed trait Tree
case class Node(range: (Int, Int), left: Tree, right: Tree) extends Tree
case class Leaf(value: Int) extends Tree
case object Empty extends Tree

object Day5 {
  def createBinaryTree(values: List[Int]): Tree = {
    values match {
      case Nil => Empty
      case x :: Nil => Leaf(x)
      case list => {
        val range = (list.head, list.last)
        val center = (list.length / 2.0).ceil.toInt - 1

        val left = list.take(center + 1)
        val right = list.drop(center + 1)

        println(s"$range: length: ${list.length} center -> $center, left -> $left, right -> $right")

        Node(range, createBinaryTree(left), createBinaryTree(right))
      }
    }
  }

  def parse(pass: String): Int = {
    val rows = createBinaryTree((0 to 127).toList)
    val columns = createBinaryTree((0 to 7).toList)

    val row = pass.take(7).toList.foldLeft(rows)((acc, char) => {
      acc match {
        case Node(_, left, right) => char match {
          case 'B' => right
          case 'F' => left
        }
        case Empty => acc
        case leaf => leaf
      }
    }) match {
      case Leaf(x) => x
    }

    val column = pass.drop(7).toList.foldLeft(columns)((acc, char) => {
      acc match {
        case Node(_, left, right) => char match {
          case 'L' => left
          case 'R' => right
        }
        case Empty => acc
        case leaf => leaf
      }
    }) match {
      case Leaf(x) => x
    }

    row * 8 + column
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day5/input.txt").getLines.toList

    val passes = lines.map(line => parse(line))

    val partOne = passes.max
    println(s"part-one: highest seat-id: $partOne")


    val partTwo = (1 to passes.max).toList.find(id => {
      if (passes.contains(id))
        false
      else
        passes.contains(id - 1) && passes.contains(id + 1)
    })

    println(s"part-two: my seat-id: $partTwo")


  }
}
