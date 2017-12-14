package aoc2017

object Day14 {
  def main(args: Array[String]) {
    val input = "ugkiagan"
    //val input = "flqrgnkx"

    val v = for (row <- 0 to 127) yield {
      val v = s"$input-$row"
      Day10.hash(v.map(c => c.toInt).toList ++ List(17, 31, 73, 47, 23))
    }

    // v map(v => (v._1, v._2, v._2.size)) foreach println

    implicit class RichInteger(val v: Int) {
      def toBinaryString4 = {
        val str = v.toBinaryString
        "0" * (4 - str.length) + str
      }
    }

    val a = v.map(v => v.flatMap(c => Integer.parseInt(c.toString, 16).toBinaryString4.split("").map(v => v.toInt)))

    println("part1: " + a.foldLeft(0)((sum, v) => sum + v.sum))

    val t = (for {
      x <- 0 to 127
      y <- 0 to 127
      if (a(x)(y) == 1)
    } yield {
      def g(dx: Int, dy: Int) =
        if (x + dx < 128 && y + dy < 128 && a(x + dx)(y + dy) == 1)
          Some((s"a($x,$y)", s"a(${x + dx},${y + dy})"))
        else
          None
      (g(0, 0) :: g(1, 0) :: g(0, 1) :: Nil).flatten
    }).flatten

    val sets = t.foldLeft[Set[Set[String]]](Set())(
      (sets, p) => {
        def findSet(id: String) = {
          val set = sets.filter(set => set.contains(id))
          if (set.size == 0)
            Set(id)
          else
            set.head
        }

        val set1 = findSet(p._1)
        val set2 = findSet(p._2)

        val newSets = sets - set1 - set2 + (set1 union set2)

        newSets
      }
    )

    println("part2: " + sets.size)
  }
}
