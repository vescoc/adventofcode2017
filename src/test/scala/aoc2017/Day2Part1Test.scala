package aoc2017

import scala.io._

import org.scalatest.FlatSpec

import aoc2017.Day2Part1._

class Day2Part1Test extends FlatSpec {
  "SpreadSheet 1x1" must "return value" in {
    val value = 5

    val spreadsheet = SpreadSheet(value.toString)

    assert(spreadsheet.rows(0)(0) == value)
  }

  "Part1 Checksum" must "return 18" in {
    val spreadsheet = SpreadSheet("5 1 9 5", "7 5 3", "2 4 6 8")

    assertResult(18) {
      solvePart1(spreadsheet)
    }
  }

  "Part2 Checksum" must "return 9" in {
    val spreadsheet = SpreadSheet("5 9 2 8", "9 4 7 3", "3 8 6 5")

    assertResult(9) {
      solvePart2(spreadsheet)
    }
  }

  "Part1 Solve" must "give result" in {
    val spreadsheet = SpreadSheet(Source.fromURL(getClass.getResource("/day2-part1-input.txt")).getLines)
    println("day2part1=" + solvePart1(spreadsheet))
  }

  "Part2 Solve" must "give result" in {
    val spreadsheet = SpreadSheet(Source.fromURL(getClass.getResource("/day2-part1-input.txt")).getLines)
    println("day2part2=" + solvePart2(spreadsheet))
  }
}
