package aoc2017

import scala.io.Source
import scala.annotation.tailrec

object Day7 {
  // suvtxzq (242) -> tdoxrnb, oanxgk
  val simpleRe = "([a-z]+)\\s[(]([0-9]+)[)]".r
  val complexRe = "([a-z]+)\\s[(]([0-9]+)[)]\\s->\\s(.*)".r

  case class Program(name: String, weight: Int, above: Set[String])
  object Program {
    def apply(line: String): Program = {
      line match {
        case simpleRe(name, weight) => Program(name, weight.toInt, Set.empty)
        case complexRe(name, weight, remainder) => Program(name, weight.toInt, remainder.split("[,\\s]+").toSet)
        case o => throw new RuntimeException(s"invalid line: $o")
      }
    }
  }

  def solve(programs: Iterator[String]) = {
    val p = programs.map(line => Program(line)).toList
    val m = p.foldLeft[Set[String]](Set.empty)((s, p) => s + p.name)
    val r = p.foldLeft(m)((s, p) => s -- p.above)

    println(s"top is ${r.head}")

    val ws: Map[String, Program] = p.map(p => p.name -> p).toMap

    def totalWeight(program: Program): Int = program.weight + program.above.foldLeft(0)((v, name) => v + totalWeight(ws(name)))

    def wt: Map[Program, Int] = p.map(p => p -> totalWeight(p)).toMap

    val v = p.map(p => (p, p.above.map(n => wt(ws(n)))))
      .filter(s => s._2.size > 1)
      .map(s => (s._1.name, s._1.above.map(n => (n, ws(n).weight, wt(ws(n)), ws(n).above.map(n => wt(ws(n)))))))
      .filter { case (n, s) => s.forall { case (n, nw, wt, s) => s.size == 1 } }
      .map { case (_, s) => s.groupBy(_._3) }
      .flatten

    val good = v.maxBy(p => p._2.size)._2.head
    val bad = v.minBy(p => p._2.size)._2.head

    println(s"bad is ${bad._1}, bad weight ${bad._2}, good weight ${bad._2 + good._3 - bad._3}")
  }

  def main(args: Array[String]) {
    solve(Source.fromFile("data/day7/input.txt").getLines)
  }
}
