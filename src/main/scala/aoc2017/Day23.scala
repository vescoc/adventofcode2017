package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day23 {
  val setIntRe = """set\s+([a-h])\s+([-0-9]+)""".r
  val setRe = """set\s+([a-h])\s+([a-h])""".r
  val subIntRe = """sub\s+([a-h])\s+([-0-9]+)""".r
  val subRe = """sub\s+([a-h])\s+([a-h])""".r
  val mulIntRe = """mul\s+([a-h])\s+([-0-9]+)""".r
  val mulRe = """mul\s+([a-h])\s+([a-h])""".r
  val jnzIntIntRe = """jnz\s+([-0-9]+)\s+([-0-9]+)""".r
  val jnzIntRe = """jnz\s+([a-h]+)\s+([-0-9]+)""".r

  def parse(lines: Iterator[String]): Iterator[(Int, (State) => State)] = {
    lines
      .map(line =>
        line match {
          case setIntRe(r, v) => (0, (state: State) => state + (r -> v.toLong))
          case setRe(r1, r2) => (0, (state: State) => state + (r1 -> state(r2)))
          case subIntRe(r, v) => (0, (state: State) => state + (r -> (state(r) - v.toLong)))
          case subRe(r1, r2) => (0, (state: State) => state + (r1 -> (state(r1) - state(r2))))
          case mulIntRe(r, v) => (1, (state: State) => state + (r -> (state(r) * v.toLong)))
          case mulRe(r1, r2) => (1, (state: State) => state + (r1 -> (state(r1) * state(r2))))
          case jnzIntIntRe(v1, v2) => (0, (state: State) => if (v1.toLong != 0) state.j(v2.toInt) else state.nop)
          case jnzIntRe(r, v) => (0, (state: State) => if (state(r) != 0) state.j(v.toInt) else state.nop)
        }
      )
  }

  case class State(pc: Int, regs: Map[String, Long]) {
    def apply(r: String): Long = regs.getOrElse(r, 0L)
    def +(p: (String, Long)) = copy(pc + 1, regs + p)
    def j(offset: Int) = copy(pc = pc + offset)
    def nop = copy(pc = pc + 1)
  }

  def main(args: Array[String]) {
    val input = parse(Source.fromFile("data/day23/input.txt").getLines).toList

    @tailrec
    def execute(step: Int = 0, current: Int = 0, state: State = State(0, Map()), limit: Int = Integer.MAX_VALUE): (Int, State, Boolean) = {
      //println(s"execute $step $state")
      if (state.pc < 0 || state.pc >= input.size)
        (current, state, true)
      else if (step >= limit)
        (current, state, false)
      else {
        val (c, istr) = input(state.pc)
        execute(step + 1, current + c, istr(state), limit)
      }
    }

    println("part 1: " + execute(limit = 1000000))

    println("part 2: " + execute(state = State(0, Map("a" -> 1))))
  }
}
