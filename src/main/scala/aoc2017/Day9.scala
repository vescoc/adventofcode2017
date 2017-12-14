package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day9 {
  object State extends Enumeration {
    type State = Value

    val Start, Group, Garbage, Ignore = Value
  }

  import State._

  def parse(stream: Iterator[Char]) = {
    stream.foldLeft((0, 0, 0, List(Start)))((acc, c) => {
      val (groupLevel, groupLevelSum, canceledSum, stack) = acc
      val state = stack.head
      c match {
        case _ if (state == Ignore) => (groupLevel, groupLevelSum, canceledSum, stack.tail)
        case '{' if (state == Start || state == Group) => (groupLevel + 1, groupLevelSum, canceledSum, Group :: stack)
        case '}' if (state == Group) => (groupLevel - 1, groupLevelSum + groupLevel, canceledSum, stack.tail)
        case '<' if (state == Start || state == Group) => (groupLevel, groupLevelSum, canceledSum, Garbage :: stack)
        case '<' | '{' | '}' if (state == Garbage) => (groupLevel, groupLevelSum, canceledSum + 1, stack)
        case '>' if (state == Garbage) => (groupLevel, groupLevelSum, canceledSum, stack.tail)
        case '!' if (state == Garbage) => (groupLevel, groupLevelSum, canceledSum, Ignore :: stack)
        case _ if (state == Garbage) => (groupLevel, groupLevelSum, canceledSum + 1, stack)
        case _ => (groupLevel, groupLevelSum, canceledSum, stack)
      }
    })
  }

  def main(args: Array[String]) {
    println(parse(Source.fromFile("data/day9/input.txt").getLines.flatMap(_.toStream)))
  }
}
