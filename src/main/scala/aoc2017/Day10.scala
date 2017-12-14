package aoc2017

import scala.io.Source

import scala.annotation.tailrec

object Day10 {
  def hash(size: Int, rounds: Int, input: List[Int]): List[Int] = {
    val l = (0 to size - 1).toArray

    def move(cp: Int, i: Int) = (cp + i) % l.size

    def swap(i1: Int, i2: Int) = {
      val v = l(i1)
      l(i1) = l(i2)
      l(i2) = v
    }

    @tailrec
    def h(cp: Int, skip: Int, input: Seq[Int]): (Int, Int) = {
      //println(s"cp=$cp skip=$skip input=$input " + l.mkString("[", ",", "]"))

      input match {
        case head :: tail  => {
          val length = head - 1
          for (i <- 0 to (length / 2))
            swap(move(cp, i), move(cp, length - i))

          h(move(cp, head + skip), skip + 1, tail)
        }
        case Nil => (cp, skip)
      }
    }

    @tailrec
    def hh(cp: Int, skip: Int, r: Int, input: Seq[Int]): (Int, Int) =
      if (r == 1)
        h(cp, skip, input)
      else {
        val (ncp, nskip) = h(cp, skip, input)
        hh(ncp, nskip, r - 1, input)
      }

    hh(0, 0, rounds, input)

    l.toList
  }

  def dense(l: List[Int]) =
    l.sliding(16, 16)
      .toList
      .map(l => l.reduceLeft((a, b) => a ^ b))
      .map(v => v.toHexString)
      .mkString("")

  def hash(input: List[Int]): String = dense(hash(256, 64, input))

  def main(args: Array[String]) {
    println("test part1=" + hash(5, 1, List(3, 4, 1, 5)))
    println("test part2 \"\"=" + dense(hash(256, 64, List(17, 31, 73, 47, 23))))
    println("test part2 hash \"\"=" + hash(List(17, 31, 73, 47, 23)))
    println("test part2 AoC 2017=" + dense(hash(256, 64, "AoC 2017".map(c => c.toInt).toList ++ List(17, 31, 73, 47, 23))))
    println("test part2 1,2,3=" + dense(hash(256, 64, "1,2,3".map(c => c.toInt).toList ++ List(17, 31, 73, 47, 23))))
    println("test part2 1,2,4=" + dense(hash(256, 64, "1,2,4".map(c => c.toInt).toList ++ List(17, 31, 73, 47, 23))))

    if (true) {
      Source
        .fromFile("data/day10/input.txt")
        .getLines
        .map(line => line
          .split("[\\s,]+")
          .map(str => str.toInt))
        .map(input => hash(256, 1, input.toList))
        .map(l => (l(0) * l(1), l))
        .foreach(r => println("result part1=" + r._1 + " " + r._2.mkString("[", ",", "]")))

      Source
        .fromFile("data/day10/input.txt")
        .getLines
        .map(line => line
          .trim
          .map(c => c.toInt)
          .toList ++ List(17, 31, 73, 47, 23))
        .map(input => dense(hash(256, 64, input)))
        .foreach(l => println(l))
    }
  }
}
