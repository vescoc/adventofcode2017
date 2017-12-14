package aoc2017

object Day14 {
  def main(args: Array[String]) {
    //val input = "ugkiagan"
    val input = "flqrgnkx"

    val v = for (row <- 0 to 127) yield Day10.hash(s"$input-$row".map(c => c.toInt).toList)

    val a: List[Int] = v.flatMap(v => v.flatMap(c => Integer.parseInt(c.toString, 16).toBinaryString)).map(c => c - '0').toList
    println(a.sum)
  }
}

