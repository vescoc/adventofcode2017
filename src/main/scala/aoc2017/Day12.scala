package aoc2017

import scala.annotation.tailrec

import scala.io.Source

import Benchmark._

object Day12 {
  def parse(line: String) = {
    val headRe = """([0-9]+)\s+<->\s+(.+)""".r

    line match {
      case headRe(id, remainder) => {
        val ids = remainder.split(",\\s+")
        for {
          p <- ids
        } yield {
          (id -> p)
        }
      }
      case o => throw new MatchError(s"invalid input string $o")
    }
  }

  def solveBySets(lines: Iterator[String]) = {
    lines
      .flatMap(line => parse(line))
      .foldLeft[Set[Set[String]]](Set())(
        (sets: Set[Set[String]], p: (String, String)) => {
          def findSet(id: String) = {
            val set = sets.filter(set => set.contains(id))
            if (set.size == 0)
              Set(id)
            else
              set.head
          }

          val set1 = findSet(p._1)
          val set2 = findSet(p._2)

          val newSets = sets - set1 - set2 + (set1 union set2)

          newSets
        }
      )
  }

  def solveByVisit(lines: Iterator[String]): Set[Set[String]] = {
    val (nodes, transactionsMap, transactions) = lines
      .flatMap(line => parse(line))
      .foldLeft[(Set[String], Map[String, Set[(String, String)]], Set[(String, String)])]((Set(), Map(), Set()))((sum, p: (String, String)) => (sum._1 + p._1 + p._2, sum._2 + (p._1 -> (sum._2.getOrElse(p._1, Set()) + p)) + (p._2 -> (sum._2.getOrElse(p._2, Set()) + p)), sum._3 + p))

    val filterMon = Mon("filter")

    @tailrec
    def span(sets: Set[Set[String]], remainder: List[(String, String)]): Set[Set[String]] = {
      @tailrec
      def visit(list: List[(String, String)], current: Set[String], remainder: List[(String, String)]): (Set[String], List[(String, String)]) = {
        //println(s"visit $list current ${current}")

        list match {
          case t :: tail => {
            val r = filterMon { remainder.filter(c => c != t) }

            visit(r.filter(c => c._1 == t._1 || c._1 == t._2 || c._2 == t._1 || c._2 == t._2) ::: tail, current + t._1 + t._2, r)
          }
          case Nil => (current, remainder)
        }
      }

      //println(s"span remainder ${remainder.size}")

      remainder match {
        case t :: tail => {
          val (set, r) = visit(List(t), Set(), tail)
          span(sets + set, r)
        }
        case Nil => sets
      }
    }

    val r = span(Set(), transactions.toList)

    println(s"info=${filterMon.info} ${transactions.size}")

    r
  }
  
  def main(args: Array[String]) {
    val lines = Source.fromFile("data/day12/input.txt").getLines.toList

    if (true)
      bench("by sets") {
        val sets = solveBySets(lines.toIterator)
        println("0: " + sets.filter(set => set.contains("0")).head.size)
        println("total: " + sets.size)
      }

    if (true)
      bench("by visit") {
        val sets = solveByVisit(lines.toIterator)
        println("0: " + sets.filter(set => set.contains("0")).head.size)
        println("total: " + sets.size)
      }
  }
}
