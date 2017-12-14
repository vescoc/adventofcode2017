package aoc2017

import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  implicit class RichList[A](val l: Seq[A]) {
    def update(idx: Int, what: A) = l.updated(idx, what)
  }

  @tailrec
  def solve(step: Int, pc: Int, program: Seq[Int]): Int = {
    //println(s"current step: $step pc: $pc program: $program")

    if (pc < 0 || pc >= program.size)
      step
    else {
      val j = program(pc)
      val newProgram = (program(pc) = j + 1)
      solve(step + 1, pc + j, newProgram)
    }
  }

  @tailrec
  def solve1(step: Int, pc: Int, program: Seq[Int]): Int = {
    // println(s"current step: $step pc: $pc program: $program")
    if (step % 100000 == 0)
      println(step)

    if (pc < 0 || pc >= program.size)
      step
    else {
      val j = program(pc)
      val off = (
        if (j >= 3)
          -1
        else
          +1
      )
      val newProgram = (program(pc) = j + off)
      solve1(step + 1, pc + j, newProgram)
    }
  }


  def main(args: Array[String]) {
    println("test: " + solve(0, 0, "0 3 0 1 -3".split("\\s+").map(_.toInt)))
    println("test 1: " + solve1(0, 0, "0 3 0 1 -3".split("\\s+").map(_.toInt)))

    if (true) {
      val program = Source.fromFile("data/day5/input.txt").getLines.map(_.toInt).toArray

      //println("solve: " + solve(0, 0, program))
      println("solve1: " + solve1(0, 0, program))
    }
  }
}
