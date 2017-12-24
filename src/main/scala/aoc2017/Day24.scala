package aoc2017

import scala.io.Source

object Day24 {
  val re = """(\d+)/(\d+)""".r

  def parse(lines: List[String]) =
    lines.map(line => line match {
      case re(p1, p2) => (p1.toInt, p2.toInt)
    })

  def strength(ports: List[(Int, Int)]) = ports.foldLeft(0)((sum, p) => sum + p._1 + p._2)
  
  def search(ports: List[(Int, Int)], maxBy: ((Int, List[(Int, Int)])) => Int): (Int, List[(Int, Int)]) = {
    def search(currentPin: Int = 0, current: List[(Int, Int)] = List(), visited: Set[List[(Int, Int)]] = Set(), remainder: Set[(Int, Int)] = ports.toSet): (Int, List[(Int, Int)]) = {
      def find(pin: Int, ports: Set[(Int, Int)]) = ports.filter(p => (p._1 == pin || p._2 == pin) && !visited.contains(p :: current))

      val candidates = find(currentPin, remainder)
      if (candidates.size == 0)
        (strength(current), current)
      else
        candidates
          .map(candidate => {
            val target = candidate :: current
            search(
              (if (candidate._1 == currentPin) candidate._2 else candidate._1),
              target,
              visited + target,
              remainder - candidate
            )
          })
          .maxBy(target => maxBy(target))
    }

    search()
  }


  def main(args: Array[String]) {
    val testLines = List(
      "0/2",
      "2/2",
      "2/3",
      "3/4",
      "3/5",
      "0/1",
      "10/1",
      "9/10"
    )
    val inputLines = Source.fromFile("data/day24/input.txt").getLines.toList

    val test = parse(testLines)
    val input = parse(inputLines)

    def part1MaxBy(target: (Int, List[(Int, Int)])) = target._1
    def part2MaxBy(target: (Int, List[(Int, Int)])) = target._2.size * 1000000 + target._1

    println("part 1 test: " + search(test, part1MaxBy))
    println("part 1 input: " + search(input, part1MaxBy))

    println("part 2 test: " + search(test, part2MaxBy))
    println("part 2 input: " + search(input, part2MaxBy))
  }
}
