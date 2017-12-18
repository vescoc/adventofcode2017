package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day18 {
  val sndRe = """snd\s+([a-z])""".r
  val setIntRe = """set\s+([a-z])\s+([-0-9]+)""".r
  val setRe = """set\s+([a-z])\s+([a-z])""".r
  val addIntRe = """add\s+([a-z])\s+([-0-9]+)""".r
  val addRe = """add\s+([a-z])\s+([a-z])""".r
  val mulIntRe = """mul\s+([a-z])\s+([-0-9]+)""".r
  val mulRe = """mul\s+([a-z])\s+([a-z])""".r
  val modIntRe = """mod\s+([a-z])\s+([-0-9]+)""".r
  val modRe = """mod\s+([a-z])\s+([a-z])""".r
  val rcvRe = """rcv\s+([a-z])""".r
  val jgzIntRe = """jgz\s+([a-z])\s+([-0-9]+)""".r
  val jgzRe = """jgz\s+([a-z])\s+([a-z])""".r
  val jgzIntIntRe = """jgz\s+([-0-9]+)\s+([-0-9]+)""".r

  case class State(pc: Int, regs: Map[String, Int] = Map(), frequency: Option[Int] = Some(0)) {
    def frequency(v: Int) = copy(frequency = Some(v), pc = pc + 1)
    def get(r: String) = regs.getOrElse(r, 0)
    def +(p: (String, Int)) = copy(regs = regs + p, pc = pc + 1)
    def inc = copy(pc = pc + 1)
    def pc(off: Int) = copy(pc = pc + off - 1)
  }

  def parse(line: String) = {
    line match {
      case sndRe(r)  => (s: State) => (s.frequency(s.get(r)), None)
      case setIntRe(r1, v) => (s: State) => (s + (r1 -> v.toInt), None)
      case setRe(r1, r2) => (s: State) => (s + (r1 -> s.get(r2)), None)
      case addIntRe(r1, v) => (s: State) => (s + (r1 -> (s.get(r1) + v.toInt)), None)
      case addRe(r1, r2) => (s: State) => (s + (r1 -> (s.get(r1) + s.get(r2))), None)
      case mulIntRe(r1, v) => (s: State) => (s + (r1 -> (s.get(r1) + v.toInt)), None)
      case mulRe(r1, r2) => (s: State) => (s + (r1 -> (s.get(r1) + s.get(r2))), None)
      case modIntRe(r1, v) => (s: State) => (s + (r1 -> (s.get(r1) % v.toInt)), None)
      case modRe(r1, r2) => (s: State) => (s + (r1 -> (s.get(r1) % s.get(r2))), None)
      case rcvRe(r) => (s: State) => if (s.get(r) == 0) (s.inc, None) else (s + (r -> s.frequency.get), Some(s.frequency.get))
      case jgzIntRe(r, v) => (s: State) => ((if (s.get(r) <= 0) s.inc else s.pc(v.toInt)), None)
      case jgzRe(r1, r2) => (s: State) => ((if (s.get(r1) <= 0) s.inc else s.pc(s.get(r2))), None)
      case jgzIntIntRe(v1, v2) => (s: State) => ((if (v1.toInt <= 0) s.inc else s.pc(v2.toInt)), None)
    }
  }

  @tailrec
  def execute(program: List[(State) => (State, Option[Int])], s: State): Option[Int] = {
    val pc = s.pc
    if (pc < 0 || pc >= program.size)
      None
    else {
      val (newState, terminate) = program(pc)(s)
      terminate match {
        case Some(frequency) => terminate
        case _ => execute(program, newState)
      }
    }
  }

  def main(args: Array[String]) {
    val test = List("set a 1", "add a 2", "mul a a", "mod a 5", "snd a", "set a 0", "rcv a", "jgz a -1", "set a 1", "jgz a -2").map(parse)
    val input = Source.fromFile("data/day18/input.txt").getLines.map(parse).toList

    println(execute(test, State(1)))
    println(execute(input, State(1)))

  }
}
