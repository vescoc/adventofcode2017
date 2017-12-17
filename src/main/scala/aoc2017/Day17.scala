package aoc2017

import scala.annotation.tailrec

import Benchmark._

object Day17 {
  @tailrec
  def spinlock(seed: Int = 3, limit: Int = 2017, count: Int = 1, idx: Int = 0, buffer: List[Int] = List(0)): (Int, List[Int]) = {
    val newIdx = (idx + seed) % buffer.size
    val (a, b) = buffer.splitAt(newIdx)
    val newBuffer = a ::: (count :: b)
    if (count == limit)
      (newIdx, newBuffer)
    else
      spinlock(seed, limit, count + 1, newIdx + 1 % newBuffer.size, newBuffer)
  }

  @tailrec
  def spinlock0(seed: Int = 3, limit: Int = 2017, count: Int = 1, idx: Int = 0, valueAt0: Int = 0, bufferSize: Int = 1): Int = {
    val newIdx = (idx + seed) % bufferSize
    val newValueAt0 = (
      if (newIdx == 0)
        count
      else
        valueAt0
    )
    val newBufferSize = bufferSize + 1
    if (count == limit)
      newValueAt0
    else
      spinlock0(seed, limit, count + 1, newIdx + 1 % newBufferSize, newValueAt0, newBufferSize)
  }
  
  def solve(seed: Int = 3) = {
    val (idx, buffer) = spinlock(seed)

    buffer(idx + 1 % buffer.size)
  }

  def main(args: Array[String]) {
    bench {
      println("part 1 test: " + solve(3))
      println("part 1: " + solve(366))
    }

    bench {
      println("part 2 test: " + spinlock0(3, 50 * 1000 * 1000))
      println("part 2: " + spinlock0(366, 50 * 1000 * 1000))
    }

    bench("recursion") {
      println("part 1: " + solve(366))
      println("part 2: " + spinlock0(366, 50 * 1000 * 1000))
    }

    bench("foldLeft") {
      val count1 = 2017
      val count2 = 50000000

      val input = 366

      val buffer = List(0)

      case class State(currentPosition: Int, buffer: List[Int])

      val result1 = (1 to count1).foldLeft(State(0, buffer))((s, i) => {
        val newPosition = (s.currentPosition + input) % s.buffer.size + 1
        val splits = s.buffer.splitAt(newPosition)
        State(newPosition, splits._1 ::: i :: splits._2)
      })

      println(result1.buffer(result1.buffer.indexOf(count1) + 1))

      val result2 = (1 to count2).foldLeft((0, Option.empty[Int]))((s, i) => {
        val newPosition = (s._1 + input) % i + 1
        newPosition match {
          case 1 => (newPosition, Some(i))
          case _ => (newPosition, s._2)
        }
      })

      println(result2._2)
    }

    {
      val mon = Mon("recursion")
      for (i <- 1 to 10)
        mon {
          solve(366)
          spinlock0(366, 50 * 1000 * 1000)
        }

      println(mon.info)
    }

    {
      val mon = Mon("foldLeft")
      for (i <- 1 to 10)
        mon {
          val count1 = 2017
          val count2 = 50000000

          val input = 366

          val buffer = List(0)

          case class State(currentPosition: Int, buffer: List[Int])

          val result1 = (1 to count1).foldLeft(State(0, buffer))((s, i) => {
            val newPosition = (s.currentPosition + input) % s.buffer.size + 1
            val splits = s.buffer.splitAt(newPosition)
            State(newPosition, splits._1 ::: i :: splits._2)
          })

          val result2 = (1 to count2).foldLeft((0, Option.empty[Int]))((s, i) => {
            val newPosition = (s._1 + input) % i + 1
            newPosition match {
              case 1 => (newPosition, Some(i))
              case _ => (newPosition, s._2)
            }
          })
        }

      println(mon.info)
    }
  }
}
