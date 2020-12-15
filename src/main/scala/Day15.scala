object Day15 {
  def search(input: List[Int], at: Int) = {
    val lookup = input.dropRight(1).zipWithIndex.toMap

    val result = (input.length - 1 until at - 1).foldLeft((input.last, lookup))((acc, index) => {
      val (num, lookup) = acc
      val nextLookup = lookup.updated(num, index)
      //      println(num, lookup.get(num), lookup)

      lookup.get(num) match {
        case None => (0, nextLookup)
        case Some(prevIndex) => {
          //          println(s"found $num before at $prevIndex current index: $index")
          val next = index - prevIndex
          (next, nextLookup)
        }
      }
    })

    result._1
  }

  def main(args: Array[String]): Unit = {
    val partOne = search(List(16,12,1,0,15,7,11), 2020)
    val partTwo = search(List(16,12,1,0,15,7,11), 30000000)

    println(s"part-one: $partOne")
    println(s"part-twp: $partTwo")
  }
}
