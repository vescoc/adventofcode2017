package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day21 {
  def at(pattern: String)(p: Int) = pattern.charAt(p).toString

  def generate(pattern: String): Set[String] = {
    @tailrec
    def generate(currents: List[String] = List(pattern), visited: Set[String] = Set()): Set[String] = {
      def flip(pattern: String): String = {
        val at = Day21.at(pattern)(_)

        pattern.size match {
          case 4 => at(2) + at(3) + at(0) + at(1)
          case 9 => at(6) + at(7) + at(8) + at(3) + at(4) + at(5) + at(0) + at(1) + at(2)
          case _ => throw new MatchError(s"invalid size for $pattern -> ${pattern.size}")
        }
      }
      def rotate(pattern: String): String = {
        val at = Day21.at(pattern)(_)

        pattern.size match {
          case 4 => at(3) + at(0) + at(1) + at(2)
          case 9 => at(6) + at(3) + at(0) + at(7) + at(4) + at(1) + at(8) + at(5) + at(2)
          case _ => throw new MatchError(s"invalid size for $pattern -> ${pattern.size}")
        }
      }

      currents match {
        case head :: tail => {
          if (!visited.contains(head))
            generate(flip(head) :: rotate(head) :: tail, visited + head)
          else
            generate(tail, visited)
        }
        case Nil => visited
      }
    }

    generate()
  }

  def main(args: Array[String]) {
    generate("...#") foreach println

    generate(".#...####") foreach println
  }
}
