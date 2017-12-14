package aoc2017

import scala.io.Source

object Day12 {
  def parse(line: String) = {
    val headRe = """([0-9]+)\s+<->\s+(.+)""".r

    line match {
      case headRe(id, remainder) => {
        val ids = remainder.split(",\\s+")
        for {
          p <- ids
        } yield {
          (id -> p)
        }
      }
      case o => throw new MatchError(s"invalid input string $o")
    }
  }

  def solve(lines: Iterator[String]) = {
    lines
      .flatMap(line => parse(line))
      .foldLeft[Set[Set[String]]](Set())(
        (sets: Set[Set[String]], p: (String, String)) => {
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
  }

  def main(args: Array[String]) {
    val sets = solve(Source.fromFile("data/day12/input.txt").getLines)
    println("0: " + sets.filter(set => set.contains("0")).head.size)
    println("total: " + sets.size)
  }
}
