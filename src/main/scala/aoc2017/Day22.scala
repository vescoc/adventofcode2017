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

    def reverse = copy(dx = -dx, dy = -dy)
  }

  case class State(map: Map[Point, Char], current: Point, direction: Direction) {
    def burst =
      if (!map.contains(current)) {
        val newDirection = direction.left
        (1, State(map + (current -> '#'), current + newDirection, newDirection))
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

      val map: Map[Point, Char] = lines.zipWithIndex.map(p => {
        val (line, y) = p
        line.zipWithIndex.flatMap(p => {
          val (c, x) = p
          if (c == '#')
            Some(Point(x, y) -> '#')
          else
            None
        })
      }).flatten.toMap

      new State(map, current, Direction())
    }
  }

  type BurstDelegate = (State) => (Int, State)

  val part1BurstDelegate =
    (state: State) => {
      if (!state.map.contains(state.current)) {
        val newDirection = state.direction.left
        (1, State(state.map + (state.current -> '#'), state.current + newDirection, newDirection))
      } else {
        val newDirection = state.direction.right
        (0, State(state.map - state.current, state.current + newDirection, newDirection))
      }
    }

  val part2BurstDelegate =
    (state: State) => {
      val (count, map, direction) = state.map.get(state.current) match {
        case Some(c) if (c == 'W') => (1, state.map + (state.current -> '#'), state.direction)
        case Some(c) if (c == '#') => (0, state.map + (state.current -> 'F'), state.direction.right)
        case Some(c) if (c == 'F') => (0, state.map - state.current, state.direction.reverse)
        case None => (0, state.map + (state.current -> 'W'), state.direction.left)
      }
      (count, State(map, state.current + direction, direction))
    }

  def burst(limit: Int, startState: State, burstDelegate: BurstDelegate = part1BurstDelegate): (Int, State) = {
    @tailrec
    def burst(step: Int = 0, count: Int = 0, state: State = startState): (Int, State) =
      if (step >= limit)
        (count, state)
      else {
        val (c, newState) = burstDelegate(state)
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

    println("part1 test 5: " + burst(5, testState))

    val (testCount, _) = burst(10000, testState)
    println(s"part1 test: $testCount")

    val (inputCount, _) = burst(10000, inputState)
    println(s"part1 input: $inputCount")

    println("part2 test 100: " + burst(100, testState, part2BurstDelegate))

    val (testCountPart2, _) = burst(10000000, testState, part2BurstDelegate)
    println(s"part2 test: $testCountPart2")

    val (inputCountPart2, _) = burst(10000000, inputState, part2BurstDelegate)
    println(s"part2 input: $inputCountPart2")
  }
}
