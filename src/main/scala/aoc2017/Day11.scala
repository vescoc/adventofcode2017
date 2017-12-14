package aoc2017

import scala.io.Source

object Day11 {
  case class Moves(n: Int, ne: Int, se: Int, s: Int, sw: Int, nw: Int) {
    def move(m: String) = m match {
      case "n" => (
        if (s > 0)
          copy(s = s - 1)
        else if (se > 0)
          copy(se = se - 1, ne = ne + 1)
        else if (sw > 0)
          copy(sw = sw - 1, nw = nw + 1)
        else
          copy(n = n + 1)
      )
      case "ne" => (
        if (sw > 0)
          copy(sw = sw - 1)
        else if (nw > 0)
          copy(nw = nw - 1, n = n + 1)
        else if (s > 0)
          copy(s = s - 1, se = se + 1)
        else
          copy(ne = ne + 1)
      )
      case "se" => (
        if (nw > 0)
          copy(nw = nw - 1)
        else if (n > 0)
          copy(n = n - 1, ne = ne + 1)
        else if (sw > 0)
          copy(sw = sw - 1, s = s + 1)
        else
          copy(se = se + 1)
      )
      case "s" => (
        if (n > 0)
          copy(n = n - 1)
        else if (ne > 0)
          copy(ne = ne - 1, se = se + 1)
        else if (nw > 0)
          copy(nw = nw - 1, sw = sw + 1)
        else
          copy(s = s + 1)
      )
      case "sw" => (
        if (ne > 0)
          copy(ne = ne - 1)
        else if (n > 0)
          copy(n = n - 1, nw = nw + 1)
        else if (se > 0)
          copy(se = se - 1, s = s + 1)
        else
          copy(sw = sw + 1)
      )
      case "nw" => (
        if (se > 0)
          copy(se = se - 1)
        else if (ne > 0)
          copy(ne = ne - 1, n = n + 1)
        else if (se > 0)
          copy(se = se - 1, s = s + 1)
        else
          copy(nw = nw + 1)
      )
    }

    def toMap = Map(
      "n" -> n,
      "ne" -> ne,
      "se" -> se,
      "s" -> s,
      "sw" -> sw,
      "nw" -> nw
    ).filter(v => v._2 > 0)

    def total = toMap.foldLeft(0)((sum, p) => sum + p._2)

    override def toString = toMap.mkString("[", ",", "]")
  }

  def solve(path: Iterator[String]) =
    path.foldLeft((Moves(0, 0, 0, 0, 0, 0), 0))((acc, m) => {
      val (moves, current) = acc
      val newMoves = moves.move(m)
      (newMoves, math.max(newMoves.total, current))
    })

  def main(args: Array[String]) {
    def parse(line: String) = line.split("[\\s+,]").toIterator

    def s(line: String) {
      val (moves, max) = solve(parse(line))

      println(line + ": " + moves.total + " - " + max + " " + moves)
    }

    s("ne,ne,ne")
    s("ne,ne,sw,sw")
    s("ne,ne,s,s")
    s("se,sw,se,sw,sw")
    s("se,se,se,se,s,s,nw")

    Source
      .fromFile("data/day11/input.txt")
      .getLines.map(line => solve(parse(line)))
      .foreach(p => println(p._1.total + " - " + p._2))
  }
}
