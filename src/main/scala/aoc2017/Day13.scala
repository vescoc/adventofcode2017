package aoc2017

import scala.annotation.tailrec

import scala.io.Source

import scala.reflect.Manifest

import aoc2017.Benchmark._

object Day13 {
  private val fwRe = """(\d+):\s+(\d+)""".r

  def parse(line: String) = line match {
    case fwRe(idx, depth) => (idx.toInt -> depth.toInt)
    case o => throw new MatchError(s"invalid line $o")
  }

  def solveParse(delay: Int, lines: List[String]) =
    lines
      .map(line => parse(line))
      .foldLeft((0, false))(
      (info, p) => {
        val (sum, caught) = info
        if ((delay + p._1) % (p._2 * 2 - 2) != 0)
          info
        else
          (sum + p._1 * p._2, true)
      }
    )

  def solve(delay: Int, lines: List[(Int, Int)]) =
    lines
      .foldLeft((0, false))(
      (info, p) => {
        val (sum, caught) = info
        if ((delay + p._1) % (p._2 * 2 - 2) != 0)
          info
        else
          (sum + p._1 * p._2, true)
      }
    )
  
  def solveOpt(delay: Int, fws: Seq[(Int, Int)]) = {
    @tailrec
    def ss(fws: Seq[(Int, Int)]): Option[() => Int] = {
      fws match {
        case head :: tail => {
          if ((delay + head._1) % (head._2 * 2 - 2) == 0) {
            def f =
              head._1 * head._2 + tail.foldLeft(0)((sum, p) => {
                if ((delay + p._1) % (p._2 * 2 - 2) != 0)
                  sum
                else
                  sum + p._1 * p._2
              }
              )

            Some(f _)
          } else
            ss(tail)
        }
        case Nil => None
      }
    }

    ss(fws)
  }

  def solveOpt(delay: Int, fwi: Iterator[(Int, Int)]) = {
    @tailrec
    def ss(fw: (Int, Int)): Option[() => Int] = {
      if ((delay + fw._1) % (fw._2 * 2 - 2) == 0) {
        def f = {
          // println(s"f called with delay=$delay")
          fw._1 * fw._2 + fwi.foldLeft(0)((sum, p) => {
            if ((delay + p._1) % (p._2 * 2 - 2) != 0)
              sum
            else
              sum + p._1 * p._2
          }
          )
        }

        Some(f _)
      } else if (fwi.hasNext)
        ss(fwi.next)
      else
        None
    }

    ss(fwi.next)
  }
  
  def main(args: Array[String]) {
    val input = Source.fromFile("data/day13/input.txt").getLines.toList
    val test = List("0: 3", "1: 2", "4: 4", "6: 4")

    val testFw = test.map(l => parse(l))
    val inputFw = input.map(l => parse(l))

    val testFwArr = testFw.toArray
    val inputFwArr = inputFw.toArray
    
    bench("part1 parse") { println("part1 test: " + solveParse(0, test)) }
    bench("part1 parse") { println("part1: " + solveParse(0, input)) }

    bench("part1") { println("part1 test no parse: " + solve(0, testFw)) }
    bench("part1") { println("part1 no parse: " + solve(0, inputFw)) }
    
    def i = new Iterator[Int] {
      var i = -1

      override def hasNext = true
      override def next = {
        i = i + 1
        i
      }
    }

    bench("part2 parse") {
      println("part2 test: " + (i
        .map(i => (i, solveParse(i, test)))
        .find(p => !p._2._2)
      ))
    }
    
    bench("part2") {
      println("part2 test: " + (i
        .map(i => (i, solve(i, testFw)))
        .find(p => !p._2._2)
      ))
    }

    if (true)
      bench("part2 parse") {
        println("part2: " + (i
          .map(i => {
            //if (i % 100000 == 0) println(i)
            (i, solveParse(i, input))
          })
          .find(p => !p._2._2)
        ))
      }

      bench("part2") {
        println("part2: " + (i
          .map(i => {
            //if (i % 100000 == 0) println(i)
            (i, solve(i, inputFw))
          })
          .find(p => !p._2._2)
        ))
      }

    bench("part1 opt") { solveOpt(0, testFw).map(v => println(s"part1 test opt: ${v()}")) }
    bench("part1 opt") { solveOpt(0, inputFw).map(v => println(s"part1 opt: ${v()}")) }

    bench("part2 opt") {
      println("part2 test opt: " + i.map(i => (i, solveOpt(i, testFw))).find {
        case (_, None) => true
        case o => false
      })
    }

    bench("part2 opt") {
      println("part2 opt: " + i.map(i => (i, solveOpt(i, inputFw))).find {
        case (_, None) => true
        case o => false
      })
    }

    bench("part2 opt iterator") {
      println("part2 test opt: " + i.map(i => (i, solveOpt(i, testFw.toIterator))).find {
        case (_, None) => true
        case o => false
      })
    }

    bench("part2 opt iterator") {
      println("part2 opt: " + i.map(i => (i, solveOpt(i, inputFw.toIterator))).find {
        case (_, None) => true
        case o => false
      })
    }

    bench("part2 opt arr") {
      println("part2 test opt: " + i.map(i => (i, solveOpt(i, testFwArr.toIterator))).find {
        case (_, None) => true
        case o => false
      })
    }

    bench("part2 opt arr") {
      println("part2 opt: " + i.map(i => (i, solveOpt(i, inputFwArr.toIterator))).find {
        case (_, None) => true
        case o => false
      })
    }
  }
}
