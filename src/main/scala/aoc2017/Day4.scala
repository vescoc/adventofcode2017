package aoc2017

import scala.io.Source
import java.io.File

object Day4 {
  def solvePart1(pw: Array[String]): Boolean = {
    pw.toSet.size == pw.length
  }

  def solvePart1(pw: String*): Boolean = {
    solvePart1(pw.toArray)
  }

  def solvePart1(pw: String): Boolean = solvePart1(pw.split("\\s+"))

  def solvePart2(pw: Array[String]): Boolean = {
    pw.map(_.permutations.toSet).combinations(2).map(v => v.head.intersect(v.last)).exists(v => v.size > 0) == false
  }

  def solvePart2(pw: String): Boolean = solvePart2(pw.split("\\s+"))

  def main(args: Array[String]) {
    val lines = (
      if (new File(args(0)).exists())
        Source.fromFile(args(0)).getLines
      else Array(args(0)).toIterator
    ).toList

    println("part1=" + lines.map(solvePart1(_)).filter(v => v).size)
    println("part2=" + lines.map(solvePart2(_)).filter(v => v).size)
  }
}
