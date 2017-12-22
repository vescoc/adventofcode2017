package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day22 {
  case class Point(x: Int, y: Int) {
    def +(direction: Direction) = copy(x + direction.dx, y + direction.dy)
  }

  case class Direction(dx: Int = 0, dy: Int = -1) {
    def right =
      if (dx > 0)
        copy(dx = 0, dy = 1)
      else if (dx < 0)
        copy(dx = 0, dy = -1)
      else if (dy > 0)
        copy(dx = -1, dy = 0)
      else
        copy(dx = 1, dy = 0)

    def left =
      if (dx > 0)
        copy(dx = 0, dy = -1)
      else if (dx < 0)
        copy(dx = 0, dy = 1)
      else if (dy > 0)
        copy(dx = 1, dy = 0)
      else
        copy(dx = -1, dy = 0)
  }

  case class State(map: Set[Point], current: Point, direction: Direction) {
    def burst =
      if (!map.contains(current)) {
        val newDirection = direction.left
        (1, State(map + current, current + newDirection, newDirection))
      } else {
        val newDirection = direction.right
        (0, State(map - current, current + newDirection, newDirection))
      }
  }
  object State {
    def apply(lines: Seq[String]) = {
      val xSize = lines.head.size
      val ySize = lines.size

      val current = Point(xSize / 2, ySize / 2)

      val map: Set[Point] = lines.zipWithIndex.map(p => {
        val (line, y) = p
        line.zipWithIndex.flatMap(p => {
          val (c, x) = p
          if (c == '#')
            Some(Point(x, y))
          else
            None
        })
      }).flatten.toSet

      new State(map, current, Direction())
    }
  }

  def burst(limit: Int, startState: State): (Int, State) = {
    @tailrec
    def burst(step: Int = 0, count: Int = 0, state: State = startState): (Int, State) =
      if (step >= limit)
        (count, state)
      else {
        val (c, newState) = state.burst
        //println(state + " -> " + newState)
        burst(step + 1, count + c, newState)
      }

    burst()
  }

  def main(args: Array[String]) {
    val test = List("..#", "#..", "...")
    val input = Source.fromFile("data/day22/input.txt").getLines.toList

    val testState = State(test)
    val inputState = State(input)

    val (testCount, _) = burst(10000, testState)
    println(s"part1 test $testCount")

    val (inputCount, _) = burst(10000, inputState)
    println(s"part1 input $inputCount")
  }
}
