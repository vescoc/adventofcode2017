package aoc2017

import scala.annotation.tailrec

import scala.io.Source

import Benchmark._

object Day16 {
  def solve(length: Int, repeat: Int, moves: Iterator[String]) = {
    val spinRe = """s(\d+)""".r
    val exchangeRe = """x(\d+)/(\d+)""".r
    val partnerRe = """p([a-z])/([a-z])""".r

    val calc = {
      val fs = moves
        .flatMap(line => line.split(","))
        .map((m) => {
          m match {
            case spinRe(sl) => {
              def t(arr: Array[Char]) = {
                val l = sl.toInt
                arr.takeRight(l) ++ arr.dropRight(l)
              }

              t _
            }
            case exchangeRe(sp1, sp2) => {
              def t(arr: Array[Char]) = {
                val p1 = sp1.toInt
                val p2 = sp2.toInt

                val v = arr(p1)

                arr(p1) = arr(p2)
                arr(p2) = v

                arr
              }

              t _
            }
            case partnerRe(sp1, sp2) => {
              def t(arr: Array[Char]) = {
                val p1 = sp1.head
                val p2 = sp2.head

                val i1 = arr.indexOf(p1)
                val i2 = arr.indexOf(p2)

                val v = arr(i1)

                arr(i1) = arr(i2)
                arr(i2) = v

                arr
              }

              t _
            }
            case o => throw new MatchError(s"invalid input $o")
          }
        }).toList

      (arr: Array[Char]) => fs.foldLeft(arr)((arr, f) => f(arr))
    }

    val start = (for (i <- 0 until length) yield ('a' + i).toChar).toArray
    val startStr = start.mkString("")

    @tailrec
    def gen(i: Int = 0, list: List[String] = List(), arr: Array[Char] = start): (Int, List[String], String) = {
      val str = arr.mkString("")
      if (i != 0 && str == startStr || i == repeat)
        (i, list, str)
      else
        gen(i + 1, str :: list, calc(arr))
    }

    val (cicle, list, str) = gen()

    if (cicle == repeat)
      str
    else
      list.reverse(repeat % cicle)
  }
  
  def main(args: Array[String]) {
    val test = List("s1,x3/4,pe/b")
    val input = Source.fromFile("data/day16/input.txt").getLines.toList

    bench {
      println("part 1 test: " + solve(5, 1, test.toIterator))
      println("part 1: " + solve(16, 1, input.toIterator))
      println("part 2 test: " + solve(5, 1000000000, test.toIterator))
      println("part 2: " + solve(16, 1000000000, input.toIterator))
    }
  }
}
