package aoc2017

import scala.io._

object Day2Part1 {
  class SpreadSheet(val cells: Array[Array[Int]]) {
    def rows = cells
  }

  object SpreadSheet {
    def apply(lines: String*) = {
      new SpreadSheet(lines.map(s => s.split("\\s+").map(v => v.toInt)).toArray)
    }

    def apply(lines: Iterator[String]) = {
      new SpreadSheet(lines.map(s => s.split("\\s+").map(v => v.toInt)).toArray)
    }
  }

  def solvePart1(spreadsheet: SpreadSheet) = {
    spreadsheet.rows.map(r => r.max - r.min).sum
  }

  def solvePart2(spreadsheet: SpreadSheet) = {
    def d(a: Int, b: Int) = 
      (if (b != 0)
        if (a % b == 0)
          a / b
        else
          0
        else 0) + (if (a != 0)
          if (b % a == 0)
            b / a
          else 0
          else 0)
    
    spreadsheet.rows.map(r => r.combinations(2).map(l => {
      val r = d(l(0), l(1))
      r
    })).map(l => l.sum).sum
  }
}
