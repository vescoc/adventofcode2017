package aoc2017

import scala.io.Source
import scala.annotation.tailrec

object Day7 {
  // suvtxzq (242) -> tdoxrnb, oanxgk
  val simpleRe = "([a-z]+)\\s[(]([0-9]+)[)]".r
  val complexRe = "([a-z]+)\\s[(]([0-9]+)[)]\\s->\\s(.*)".r

  trait Program {
    val name: String
    val weight: Int
    val above: Set[String]
    
    def children: Set[_ <: Program]

    override def toString = s"$name($weight)"
  }

  def solve(programs: Iterator[String]): Set[_ <: Program] = {
    abstract class AbstractProgram(val name: String, val weight: Int, val above: Set[String]) extends Program {
      override def children: Set[AbstractProgram]
    }

    type Lookup = (String) => AbstractProgram
    
    lazy val (lookup: Lookup, roots: Set[AbstractProgram]) = {
      object Program {
        def apply(line: String): AbstractProgram = {
          line match {
            case simpleRe(name, weight) => new AbstractProgram(name, weight.toInt, Set.empty) {
              override def children: Set[AbstractProgram] = Set.empty
            }
            case complexRe(name, weight, remainder) => new AbstractProgram(name, weight.toInt, remainder.split("[,\\s]+").toSet) {
              override def children: Set[AbstractProgram] = above.map(lookup)
            }
          }
        }
      }

      val (map, added, removed) = programs
        .map(line => Program(line))
        .foldLeft[(Map[String, AbstractProgram], Set[String], Set[String])]((Map(), Set(), Set()))((b, a) =>
          (b._1 + (a.name -> a), b._2 + a.name, b._3 ++ a.above)
        )

      ((name: String) => map(name), (added -- removed).map(name => map(name)))
    }

    roots
  }

  def solve2(roots: Set[_ <: Program]): Set[(_ <: Program, Int)] = {
    trait TotalWeight {
      val totalWeight: Int
    }

    abstract class AbstractProgram(delegate: Program) extends Program with TotalWeight {
      override val name = delegate.name
      override val weight = delegate.weight
      override val above = delegate.above
      def children: Set[AbstractProgram]
    }

    def transform(program: Program): AbstractProgram = {
      new AbstractProgram(program) {
        override def children: Set[AbstractProgram] = program.children.map(child => transform(child))

        val totalWeight = weight + children.foldLeft(0)((sum, child) => sum + child.totalWeight)
      }
    }

    @tailrec
    def find(program: AbstractProgram, targetTotalWeight: Int): Option[(AbstractProgram, Int)] = {
      val groups: Map[Int, Set[AbstractProgram]] = program.children.groupBy(_.totalWeight)
      if (groups.size < 2)
        Some((program, targetTotalWeight - program.children.foldLeft(0)((sum, child) => sum + child.totalWeight)))
      else {
        val (k: Int, p: Set[AbstractProgram]) = groups.foldLeft[(Int, Set[AbstractProgram])]((0, Set()))((b, a) => a._2.size match {
          case 1 => (b._1, a._2)
          case _ => (a._1, b._2)
        })

        find(p.head, k)
      }
    }

    roots.map(transform).flatMap(program => find(program, program.totalWeight))
  }

  def main(args: Array[String]) {
    val roots = solve(Source.fromFile("data/day7/input.txt").getLines)

    roots.foreach(p => println("root program " + p + " children " + p.children.mkString("[", ",", "]")))

    solve2(roots).foreach(p => println(s"bad program ${p._1} good weight ${p._2}"))
  }
}
