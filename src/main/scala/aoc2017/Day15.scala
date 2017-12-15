package aoc2017

import scala.annotation.tailrec

object Day15 {
  class Generator(start: Long, factor: Long, multiple: Long = 1L) extends Iterator[Long] {
    private var current = start

    def hasNext = true

    def next: Long = n

    @tailrec
    private def n: Long = {
      current = current * factor % 2147483647L
      if (current % multiple == 0)
        current
      else
        n
    }
  }

  def main(args: Array[String]) {
    val test = false

    def generatorMatch(p: (Long, Long)) = (p._1 & 0xFFFFL) == (p._2 & 0xFFFFL)

    {
      val a = new Generator((if (test) 65 else 516), 16807)
      val b = new Generator((if (test) 8921 else 190), 48271)

      println("part 1 small: " + a.zip(b).take(5).filter(generatorMatch).size)
    }

    {
      val a = new Generator((if (test) 65 else 516), 16807)
      val b = new Generator((if (test) 8921 else 190), 48271)

      println("part 1: " + a.zip(b).take(40000000).filter(generatorMatch).size)
    }
    
    {
      val a = new Generator((if (test) 65 else 516), 16807, 4)
      val b = new Generator((if (test) 8921 else 190), 48271, 8)

      println("part 2 small: " + a.zip(b).take(5).filter(generatorMatch).size)
    }

    {
      val a = new Generator((if (test) 65 else 516), 16807, 4)
      val b = new Generator((if (test) 8921 else 190), 48271, 8)

      println("part 2: " + a.zip(b).take(5000000).filter(generatorMatch).size)
    }
  }
}
