package aoc2017

import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]) {
    def solve(n: Int) = {
      @tailrec
      def dist(c: (Int, Int)): (Int, Int) = {
        if (n <= c._2 * c._2)
          c
        else
          dist(c._1 + 1, c._2 + 2) 
      }

      val (c, d) = dist((0, 1))

      val p = d - 2

      val r = n - p * p

      val l = d / 2

      val coord = (
        if (r <= d - 1)
          ("r", c, -(r - l))
        else if (r <= d - 1 + d - 1)
          ("u", -(r - (d - 1) - l), -c)
        else if (r <= d - 1 + d - 1 + d - 1)
          ("l", -c, r - (d - 1 + d - 1) - l)
        else ("d", r - (d - 1 + d - 1 + d - 1) - l, c)
      )

      coord
    }

    def p(n: Int) {
      val coord = solve(n)

      println(n + "=" + coord + " dist=" + (Math.abs(coord._2) + Math.abs(coord._3)))
    }

    val input = 265149

    p(input)

    @tailrec
    def solve1(v: Int, idx: Int, m: Map[(Int, Int), Int]): Int = {
      if (v > input)
        v
      else {
        val coord = solve(idx)

        val nv = (
          for {
            x <- List(-1, 0, 1)
            y <- List(-1, 0, 1)
          } yield {
            val c = (
              if (x == 0 && y == 0)
                0
              else
                m.getOrElse((coord._2 + x, coord._3 + y), 0)
            )
            c
          }
        ).sum

        //println("nv=" + nv + " idx=" + (idx + 1) + " coord=" + coord)

        solve1(nv, idx + 1, m + ((coord._2, coord._3) -> nv))
      }
    }

    println(solve1(1, 2, Map((0, 0) -> 1)))
  }
}
