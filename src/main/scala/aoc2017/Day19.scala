package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day19 {
  object Direction extends Enumeration {
    type Direction = Value

    val DOWN, LEFT, RIGHT, UP = Value
  }

  import Direction._

  def findStart(arr: Array[Array[Char]]) = (0, arr(0).indexOf('|'))

  def walk(arr: Array[Array[Char]], str: String, count: Int, current: (Int, Int), direction: Direction): Option[String] = {
    //println(s"current=$str $current $direction $count")

    if (count == 0)
      Some(str)
    else
      if (current._1 < 0 || current._1 >= arr.length || current._2 < 0 || current._2 > arr(current._1).length)
      None
    else
      arr(current._1)(current._2) match {
        case ' ' => None
        case '|' if (direction == DOWN) => walk(arr, str, count - 1, (current._1 + 1, current._2), DOWN)
        case '|' if (direction == UP) => walk(arr, str, count - 1, (current._1 - 1, current._2), UP)
        case '|' if (direction == LEFT) => walk(arr, str, count, (current._1, current._2 - 1), LEFT)
        case '|' if (direction == RIGHT) => walk(arr, str, count, (current._1, current._2 + 1), RIGHT)
        case '-' if (direction == RIGHT) => walk(arr, str, count - 1, (current._1, current._2 + 1), RIGHT)
        case '-' if (direction == LEFT) => walk(arr, str, count - 1, (current._1, current._2 - 1), LEFT)
        case '-' if (direction == UP) => walk(arr, str, count, (current._1 - 1, current._2), UP)
        case '-' if (direction == DOWN) => walk(arr, str, count, (current._1 + 1, current._2), DOWN)
        case '+' if (direction == DOWN || direction == UP) => walk(arr, str, count - 1, (current._1, current._2 + 1), RIGHT) match {
          case r @ Some(_) => r
          case r @ _ => walk(arr, str, count - 1, (current._1, current._2 - 1), LEFT)
        }
        case '+' if (direction == RIGHT || direction == LEFT) => walk(arr, str, count - 1, (current._1 + 1, current._2), DOWN) match {
          case r @ Some(_) => r
          case r @ _ => walk(arr, str, count - 1, (current._1 - 1, current._2), UP)
        }
        case c if (c == '+' || c == '|' || c == '+') => throw new MatchError(s"handle '$c' direction=$direction")
        case l if (direction == DOWN) => walk(arr, str + l, count - 1, (current._1 + 1, current._2), DOWN)
        case l if (direction == RIGHT) => walk(arr, str + l, count - 1, (current._1, current._2 + 1), RIGHT)
        case l if (direction == UP) => walk(arr, str + l, count - 1, (current._1 - 1, current._2), UP)
        case l if (direction == LEFT) => walk(arr, str + l, count - 1, (current._1, current._2 - 1), LEFT)
      }
  }

  def main(args: Array[String]) {
    {
      val test = List(
        "     |          ",
        "     |  +--+    ",
        "     A  |  C    ",
        " F---|----E|--+ ",
        "     |  |  |  D ",
        "     +B-+  +--+ "
      )

      val arr: Array[Array[Char]] = test.map(line => line.map(c => c).toArray).toArray

      val target = arr.flatten.filter(c => c != ' ').size

      val start = findStart(arr)

      println(start)

      println(walk(arr, "", target, start, DOWN))
    }

    {
      val arr = Source.fromFile("data/day19/input.txt").getLines.toArray.map(line => line.map(c => c).toArray)

      val target = arr.flatten.filter(c => c != ' ').size

      val start = findStart(arr)

      println(start)

      println(walk(arr, "", target, start, DOWN))
    }
  }
}
