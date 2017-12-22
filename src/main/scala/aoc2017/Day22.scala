package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day22 {
  case class Point(x: Int, y: Int)

  case class Direction(x: Int = 0, y: Int = -1) {
    def right =
      if (x > 0)
        copy(x = 0, y = 1)
      else if (x < 0)
        copy(x = 0, y = -1)
      else if (y > 0)
        copy(x = -1, y = 0)
      else
        copy(x = 1, y = 0)

    def left =
      if (x > 0)
        copy(x = 0, y = -1)
      else if (x < 0)
        copy(x = 0, y = 1)
      else if (y > 0)
        copy(x = 1, y = 0)
      else
        copy(x = -1, y = 0)
  }

  case class State(map: Set[Point], current: Point, direction: Direction) {
    def burst = {
      map.get(current) match {
        case None => 
      }
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
        burst(step + 1, count + c, newState)
      }

    burst()
  }

  def main(args: Array[String]) {
    val test = List("..#", "#..", "...")

    val state = State(test)

    println(state)
  }
}
