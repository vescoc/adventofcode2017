package aoc2017

import scala.annotation.tailrec

import Benchmark._

object Day15 {
  def main(args: Array[String]) {
    val test = false

    def generatorMatch(p: (Long, Long)) = (p._1 & 0xFFFFL) == (p._2 & 0xFFFFL)

    bench("var usign") {
      class Generator(start: Long, factor: Long, multiple: Long = 1L) extends Iterator[Long] {
        private var current = start

        def hasNext = true

        def next: Long = n

        @tailrec
        private def n: Long = {
          current = current * factor % 2147483647L
          if (current % multiple == 0)
            current
          else
            n
        }
      }

      {
        val a = new Generator((if (test) 65 else 516), 16807)
        val b = new Generator((if (test) 8921 else 190), 48271)

        println("part 1 small: " + a.zip(b).take(5).filter(generatorMatch).size)
      }

      {
        val a = new Generator((if (test) 65 else 516), 16807)
        val b = new Generator((if (test) 8921 else 190), 48271)

        println("part 1: " + a.zip(b).take(40000000).filter(generatorMatch).size)
      }
      
      {
        val a = new Generator((if (test) 65 else 516), 16807, 4)
        val b = new Generator((if (test) 8921 else 190), 48271, 8)

        println("part 2 small: " + a.zip(b).take(5).filter(generatorMatch).size)
      }

      {
        val a = new Generator((if (test) 65 else 516), 16807, 4)
        val b = new Generator((if (test) 8921 else 190), 48271, 8)

        println("part 2: " + a.zip(b).take(5000000).filter(generatorMatch).size)
      }
    }

    bench("foldLeft") {
      val i = for (
        _ <- new Iterator[Unit] {
          def hasNext = true
          def next = Unit
        }
      ) yield true

      case class Generator(start: Long, factor: Long, multiple: Long = 1L) {
        @tailrec
        def next(current: Long): Long = {
          val _current = current * factor % 2147483647L
          if (_current % multiple == 0)
            _current
          else
            next(_current)
        }
      }

      def ff(i: Iterator[Boolean], a: Generator, b: Generator) =
        i.foldLeft((a.start, b.start, 0))((sum, _) => {
          val ca = a.next(sum._1)
          val cb = b.next(sum._2)

          if (generatorMatch((ca, cb)))
            (ca, cb, sum._3 + 1)
          else
            (ca, cb, sum._3)
        })._3

      {
        val a = Generator((if (test) 65 else 516), 16807)
        val b = Generator((if (test) 8921 else 190), 48271)

        def fx(idx: Int) = ff(i.take(idx), a, b)
      
        println("part 1 small: " + fx(5))
        println("part 1: " + fx(40000000))
      }

      {
        val a = Generator((if (test) 65 else 516), 16807, 4)
        val b = Generator((if (test) 8921 else 190), 48271, 8)

        def fx(idx: Int) = ff(i.take(idx), a, b)
      
        println("part 2 small: " + fx(5))
        println("part 2: " + fx(5000000))
      }
    }

    bench("rec") {
      case class Generator(start: Long, factor: Long, multiple: Long = 1L) {
        @tailrec
        def next(current: Long): Long = {
          val _current = current * factor % 2147483647L
          if (_current % multiple == 0)
            _current
          else
            next(_current)
        }
      }

      def ff(idx: Int, currentA: Long, nextA: (Long) => Long, currentB: Long, nextB: (Long) => Long) = {
        @tailrec
        def f(idx: Int, sum: Int, currentA: Long, currentB: Long): Int =
          if (idx == 0)
            sum
          else {
            val ca = nextA(currentA)
            val cb = nextB(currentB)
          
            val s = {
              if (generatorMatch((ca, cb)))
                sum + 1
              else
                sum
            }

            f(idx - 1, s, ca, cb)
          }

        f(idx, 0, currentA, currentB)
      }

      {
        val a = Generator((if (test) 65 else 516), 16807)
        val b = Generator((if (test) 8921 else 190), 48271)

        def fx(idx: Int) = ff(idx, a.start, a.next, b.start, b.next)

        println("part 1 small: " + fx(5))
        println("part 1: " + fx(40000000))
      }

      {
        val a = Generator((if (test) 65 else 516), 16807, 4)
        val b = Generator((if (test) 8921 else 190), 48271, 8)

        def fx(idx: Int) = ff(idx, a.start, a.next, b.start, b.next)

        println("part 2 small: " + fx(5))
        println("part 2: " + fx(5000000))
      }
    }
  }
}
