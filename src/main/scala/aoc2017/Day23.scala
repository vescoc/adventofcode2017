package aoc2017

import scala.annotation.tailrec

import scala.io.Source

import Benchmark._

object Day23 {
  val setIntRe = """set\s+([a-h])\s+([-0-9]+)""".r
  val setRe = """set\s+([a-h])\s+([a-h])""".r
  val subIntRe = """sub\s+([a-h])\s+([-0-9]+)""".r
  val subRe = """sub\s+([a-h])\s+([a-h])""".r
  val mulIntRe = """mul\s+([a-h])\s+([-0-9]+)""".r
  val mulRe = """mul\s+([a-h])\s+([a-h])""".r
  val jnzIntIntRe = """jnz\s+([-0-9]+)\s+([-0-9]+)""".r
  val jnzIntRe = """jnz\s+([a-h]+)\s+([-0-9]+)""".r
  val jgzIntRe = """jgz\s+([a-h]+)\s+([-0-9]+)""".r

  def parse(lines: Seq[String]): Seq[(State) => (State, String)] = {
    lines
      .map(line =>
        line match {
          case setIntRe(r, v) => (state: State) => (state + (r -> v.toLong), "set")
          case setRe(r1, r2) => (state: State) => (state + (r1 -> state(r2)), "set")
          case subIntRe(r, v) => (state: State) => {
            if (r == "h")
              println(s"setting h ${state(r) - v.toLong}")
            (state + (r -> (state(r) - v.toLong)), "sub")
          }
          case subRe(r1, r2) => (state: State) => (state + (r1 -> (state(r1) - state(r2))), "sub")
          case mulIntRe(r, v) => (state: State) => (state + (r -> (state(r) * v.toLong)), "mul")
          case mulRe(r1, r2) => (state: State) => (state + (r1 -> (state(r1) * state(r2))), "mul")
          case jnzIntIntRe(v1, v2) => (state: State) => if (v1.toLong != 0) (state.j(v2.toInt), "jnz") else (state.nop, "nop")
          case jnzIntRe(r, v) => (state: State) => if (state(r) != 0) (state.j(v.toInt), "jnz") else (state.nop, "nop")
          case jgzIntRe(r, v) => (state: State) => if (state(r) > 0) (state.j(v.toInt), "jgz") else (state.nop, "nop")
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
    val cStart = 99 * 100 + 100000
    val cEnd = cStart + 17000

    val lines = Source.fromFile("data/day23/input.txt").getLines.toList
    val lines1 = List(
      "set b " + cStart,
      "set c " + cEnd,
      "jnz a 2",
      "jnz 1 5",
      "mul b 100",
      "sub b -100000",
      "set c b",
      "sub c -17000",
      "set f 1",
      "set d 2",
      "set e 2",
      "set g d",
      "mul g e",
      "sub g b",
      "jnz g 2",
      "set f 0",
      "sub e -1",
      "set g e",
      "sub g b",
      "jnz g -8",
      "sub d -1",
      "set g d",
      "sub g b",
      "jnz g -13",
      "jnz f 2",
      "sub h -1",
      "set g b",
      "sub g c",
      "jnz g 2",
      "jnz 1 3",
      "sub b -17",
      "jnz 1 -23")

    val lines1Opt = List(
      "set b " + cStart,
      "set c " + cEnd,
      "jnz a 2",
      "jnz 1 5",
      "mul b 100",
      "sub b -100000",
      "set c b",
      "sub c -17000",
      "set f 1",
      "set d 2",
      "set e 2",
      "set g d",
      "mul g e",
      "sub g b",
      "jnz g 3",
      "set f 0",
      "jnz 1 9",
      "sub e -1",
      "set g e",
      "sub g b",
      "jnz g -9",
      "sub d -1",
      "set g d",
      "sub g b",
      "jnz g -14",
      "jnz f 2",
      "sub h -1",
      "set g b",
      "sub g c",
      "jnz g 2",
      "jnz 1 3",
      "sub b -17",
      "jnz 1 -24")

    val linesOpt = List(
      "set b 99",
      "set c b",
      "jnz a 2",
      "jnz 1 5",
      "mul b 100",
      "sub b -100000",
      "set c b",
      "sub c -17000",
      "set f 1",
      "set d 2",
      "set e 2",
      "set g d",
      "mul g e",
      "sub g b",
      "jnz g 3",
      "set f 0",
      "jnz 1 10",
      "jgz g 5",
      "sub e -1",
      "set g e",
      "sub g b",
      "jnz g -10",
      "sub d -1",
      "set g d",
      "sub g b",
      "jnz g -15",
      "jnz f 2",
      "sub h -1",
      "set g b",
      "sub g c",
      "jnz g 2",
      "jnz 1 3",
      "sub b -17",
      "jnz 1 -25")
    
    val input = parse(lines)
    val input1 = parse(lines1)
    val inputOpt = parse(linesOpt)
    val input1Opt = parse(lines1Opt)

    val mulIdx = lines.zipWithIndex.filter(p => p._1.startsWith("mul")).map(p => p._2)

    @tailrec
    def execute(input: Seq[(State) => (State, String)] = input, step: Long = 0, map: Map[Int, Map[String, Long]] = Map(), state: State = State(0, Map()), limit: Long = Integer.MAX_VALUE): (State, Boolean, Map[Int, Map[String, Long]]) = {
      if (state.pc < 0 || state.pc >= input.size)
        (state, true, map)
      else if (step >= limit)
        (state, false, map)
      else {
        val pc = state.pc
        val istr = input(pc)
        val (newState, debug) = istr(state)
        val newMap = map + (pc -> {
          val m = map.getOrElse(pc, Map())
          m + (debug -> (m.getOrElse(debug, 0L) + 1L))
        })
        execute(input, step + 1, newMap, newState, limit)
      }
    }

    bench("part1") {
      val part1 = execute()

      println("part 1: " + part1._3.filter(p => mulIdx.contains(p._1)).flatMap(p => p._2.map(p => p._2)).sum)

      println("part 1 execution " + part1._2 + " " + part1._1)
      lines.zipWithIndex.map(p => s"${p._2}: ${p._1} --- ${part1._3.get(p._2)}") foreach println
    }

    bench("part1 1") {
      val part1 = execute(input1, limit = 5000000)

      println("part 1 execution " + part1._2 + " " + part1._1)
      lines1.zipWithIndex.map(p => s"${p._2}: ${p._1} --- ${part1._3.get(p._2)}") foreach println
    }

    bench("part1 opt 1") {
      val part = execute(input1Opt, limit = 5000000)

      println("part opt 1 execution " + part._2 + " " + part._1)
      lines1Opt.zipWithIndex.map(p => s"${p._2}: ${p._1} --- ${part._3.get(p._2)}") foreach println
    }
    
    if (false)
      bench("part 2") {
        val part2 = execute(state = State(0, Map("a" -> 1)), limit = Integer.MAX_VALUE.toLong * 10)

        println("part 2 execution " + part2._2 + " " + part2._1)
        lines.zipWithIndex.map(p => s"${p._2}: ${p._1} --- ${part2._3.get(p._2)}") foreach println
      }

    if (true)
      bench("part 2 opt") {
        val part = execute(inputOpt, state = State(0, Map("a" -> 1)))

        println("part 2 opt execution " + part._2 + " " + part._1)
        linesOpt.zipWithIndex.map(p => s"${p._2}: ${p._1} --- ${part._3.get(p._2)}") foreach println
      }

    bench("part 2 native scala") {
      val l = if (false) {
        var h = 0
        for (b <- cStart to cEnd by 17) {
          var f = 1
          for (d <- 2 until b) {
            for (e <- 2 until b) {
              if (e * d == b)
                f = 0
            }
          }
          if (f == 0)
            h = h + 1
        }

        h
      } else {
        @tailrec
        def cb(current: Int = 0, b: Int = cStart): Int = {
          @tailrec
          def cd(d: Int = 2): Boolean = {
            @tailrec
            def ce(e: Int = 2): Boolean = {
              val r = d * e
              if (r == b)
                false
              else if (r > b)
                true
              else if (e == b)
                true
              else
                ce(e + 1)
            }

            if (d == b)
              true
            else
              ce() && cd(d + 1)
          }
          
          if (b > cEnd)
            current
          else
            cb(current + (if (cd()) 0 else 1), b + 17)
        }

        cb()
      }

      println(s"h=$l")
    }
  }
}
