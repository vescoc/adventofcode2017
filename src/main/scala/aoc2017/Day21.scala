package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day21 {
  def at(pattern: String)(p: Int) = pattern.charAt(p).toString

  def generate(pattern: String): Set[String] = {
    @tailrec
    def generate(currents: List[String] = List(pattern), visited: Set[String] = Set()): Set[String] = {
      def flip(pattern: String): String = {
        val at = Day21.at(pattern)(_)

        pattern.size match {
          case 4 => at(2) + at(3) + at(0) + at(1)
          case 9 => at(6) + at(7) + at(8) + at(3) + at(4) + at(5) + at(0) + at(1) + at(2)
          case _ => throw new MatchError(s"invalid size for $pattern -> ${pattern.size}")
        }
      }
      def rotate(pattern: String): String = {
        val at = Day21.at(pattern)(_)

        pattern.size match {
          case 4 => at(3) + at(0) + at(1) + at(2)
          case 9 => at(6) + at(3) + at(0) + at(7) + at(4) + at(1) + at(8) + at(5) + at(2)
          case _ => throw new MatchError(s"invalid size for $pattern -> ${pattern.size}")
        }
      }

      currents match {
        case head :: tail => {
          if (!visited.contains(head))
            generate(flip(head) :: rotate(head) :: tail, visited + head)
          else
            generate(tail, visited)
        }
        case Nil => visited
      }
    }

    generate()
  }

  def parse(lines: List[String]) = {
    val re = """([.#/]+)\s+=>\s+([.#/]+)""".r

    def cleanup(str: String) = str.replaceAll("/", "")

    lines
      .map(line => line match {
        case re(pattern, image) => {
          val img = cleanup(image)
          generate(cleanup(pattern)).map(pattern => (pattern -> img))
        }
      })
      .flatten
      .toMap
  }

  def display(str: String) = {
    val l = math.sqrt(str.size).toInt
    str.grouped(l) foreach println
  }

  def partition(image: String) = {
    val size = image.size
    val l = math.sqrt(size).toInt

    def partition(s: Int, x: Int, y: Int) =
      (for (i <- 0 until s) yield image.slice(y * l * s + x * s + i * l, y * l * s + x * s + i * l + s)).mkString

    def iterate(pl: Int) = 
      (
        for {
          y <- 0 until pl
          x <- 0 until pl
        } yield partition(pl, x, y)
      ).toList

    if (l <= 3)
      List(image)
    else if (l % 2 == 0) {
      iterate(l / 2)
    } else if (l % 3 == 0) {
      iterate(l / 3)
    } else
      throw new MatchError(s"invalid value for $l")
  }

  def reassemble(list: List[String]) = {
    val image = list.mkString

    val l = math.sqrt(list.head.size).toInt
    val s = math.sqrt(list.size).toInt

    (
      for {
        y <- 0 until s
        i <- 0 until l
        x <- 0 until s
      } yield list(x + y * s).slice(i * l, i * l + l)
    ).mkString
  }

  def draw(book: Map[String, String], limit: Int, image: String) = {
    @tailrec
    def iterate(step: Int = 0, current: String = image): String = {
      println(s"step=$step")
      display(current)

      if (step == limit)
        current
      else {
        val partitions = partition(current)
        println(partitions)
        iterate(step + 1, reassemble(partition(current).map(image => book(image))))
      }
    }

    iterate()
  }

  def main(args: Array[String]) {
    val book = parse(Source.fromFile("data/day21/input.txt").getLines.toList)

    val start = ".#...####"

    book filter(_._1.size == 4) foreach println

    println("")

    display(start)

    println("")

    display(book(start))

    println("")

    display(draw(book, 5, start))
  }
}
