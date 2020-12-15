import scala.io.Source
import scala.util.{Success, Try}

object Day13 {
  def partOne(lines: Seq[String]) = {
    val List(estimate, busses, _*) = lines
    val from = estimate.toInt

    println(busses.split(',').filter(_ != "x").toList.map(i => {
      val bus = i.toInt
      val possible = math.ceil(from.toDouble / i.toDouble).toInt * bus
      val diff = possible - from

      (bus, diff)
    }).minBy(i => i._2) match {
      case (bus, diff) => bus * diff
    })
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13/input.txt").getLines.toList


    // 7,13,x,x,59,x,31,19
    println(chineseRemainder(List[BigInt](17,7,13,19), List[BigInt](0, 1, 2, 3)))



  }

  def chineseRemainder(n: List[BigInt], a: List[BigInt]): Option[BigInt] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[BigInt], a: List[BigInt], sm: BigInt): BigInt = {
      def mulInv(a: BigInt, b: BigInt): BigInt = {
        def loop(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }
}
