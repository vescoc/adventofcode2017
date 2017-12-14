package aoc2017

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  def solve(lines: Iterator[String]) = {
    // ebu inc 626 if iq < 0
    val lineRe = "([a-z]+)\\s+((?:inc)|(?:dec))\\s+([0-9-]+)\\s+if\\s+([a-z]+)\\s+([=!<>]+)\\s+([0-9-]+)".r

    type Func = (Map[String, Int], Int) => (Map[String, Int], Int)

    def makeOp(r: String, op: String, value: Int): Func = {
      val o = op match {
        case "inc" => (a: Int, b: Int) => a + b
        case "dec" => (a: Int, b: Int) => a - b
      }
      (reg: Map[String, Int], maxValue: Int) => {
        val v = o(reg.getOrElse(r, 0), value)
        (reg + (r -> v), math.max(maxValue, v))
      }
    }

    def makeCmp(rcond: String, cmpcond: String, valuecond: Int, op: Func): Func = {
      val cond = cmpcond match {
        case "<" => (a: Int, b: Int) => a < b
        case ">" => (a: Int, b: Int) => a > b
        case "<=" => (a: Int, b: Int) => a <= b
        case ">=" => (a: Int, b: Int) => a >= b
        case "==" => (a: Int, b: Int) => a == b
        case "!=" => (a: Int, b: Int) => a != b
      }

      (reg: Map[String, Int], maxValue: Int) =>
      if (cond(reg.getOrElse(rcond, 0), valuecond))
        op(reg, maxValue)
      else
        (reg, maxValue)
    }

    case class OpFunc(cmp: Func) {
      def execute(reg: Map[String, Int], maxValue: Int) = cmp(reg, maxValue)
    }

    case class Op(r: String, op: String, value: Int, rcond: String, cmpcond: String, valuecond: Int) {
      def execute(reg: Map[String, Int], maxValue: Int) = {
        def doop = {
          val v = op match {
            case "inc" => reg.getOrElse(r, 0) + value
            case "dec" => reg.getOrElse(r, 0) - value
          }

          (reg + (r -> v), math.max(v, maxValue))
        }

        cmpcond match {
          case "<" if (reg.getOrElse(rcond, 0) < valuecond) => doop
          case ">" if (reg.getOrElse(rcond, 0) > valuecond) => doop
          case "<=" if (reg.getOrElse(rcond, 0) <= valuecond) => doop
          case ">=" if (reg.getOrElse(rcond, 0) >= valuecond) => doop
          case "==" if (reg.getOrElse(rcond, 0) == valuecond) => doop
          case "!=" if (reg.getOrElse(rcond, 0) != valuecond) => doop
          case _ => (reg, maxValue)
        }
      }      
    }

    val (reg, maxValue) = if (false)
      lines.map {
        case lineRe(r, op, value, rcond, cmpcond, valuecond) => Op(r, op, value.toInt, rcond, cmpcond, valuecond.toInt)
        case o => throw new RuntimeException(s"invalid line $o")
      }
        .foldLeft[(Map[String, Int], Int)]((Map(), Int.MinValue))((info, op) => op.execute(info._1, info._2))
    else
      lines.map {
        case lineRe(r, op, value, rcond, cmpcond, valuecond) => OpFunc(makeCmp(rcond, cmpcond, valuecond.toInt, makeOp(r, op, value.toInt)))
        case o => throw new RuntimeException(s"invalid line $o")
      }
        .foldLeft[(Map[String, Int], Int)]((Map(), Int.MinValue))((info, op) => op.execute(info._1, info._2))

    (reg.maxBy(p => p._2), maxValue)
  }

  def main(args: Array[String]) {
    println(solve((Source.fromFile("data/day8/input.txt").getLines)))
  }
}
