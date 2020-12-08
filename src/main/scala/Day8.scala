import scala.io.Source

case class Instruction(cmd: String, num: Int)

object Day8 {
  def run(instructions: List[Instruction], history: List[Int], acc: Int, index: Int): (Int, List[Int]) = {
    if (history.contains(index) || index >= instructions.length)
      (acc, history.reverse)
    else {
      val ins = instructions(index)
      val updated = index :: history

      ins.cmd match {
        case "acc" => run(instructions, updated, acc + ins.num, index + 1)
        case "jmp" => run(instructions, updated, acc, index + ins.num)
        case _ => run(instructions, updated, acc, index + 1)
      }
    }
  }

  def partOne(instructions: List[Instruction]) = {
    val (acc, _) = run(instructions, List(), 0, 0)
    println(s"part-one: $acc")
  }

  def partTwo(instructions: List[Instruction]) = {
    val indexes = instructions.zipWithIndex.filter(i => i._1.cmd == "nop" || i._1.cmd == "jmp").map(i => i._2)

    def test(list: List[Int]): Int = list match {
      case head :: tail => {
        val orig = instructions(head)
        val updated = instructions.updated(head, orig match {
          case Instruction("nop", num) => Instruction("jmp", num)
          case Instruction("jmp", num) => Instruction("nop", num)
        })

        val (acc, history) = run(updated, List(), 0, 0)

        if (history(history.length - 2) == updated.length - 2) {
          acc
        } else {
          test(tail)
        }
      }
    }

    println(test(indexes))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day8/input.txt").getLines.toList

    val InstructionRegex = "(.+?) (.+)".r
    val instructions = lines.map {
      case InstructionRegex(cmd, num) => Instruction(cmd, num.toInt)
    }

    partOne(instructions)
    partTwo(instructions)
  }
}
