package aoc2017

import scala.io.Source

import scala.language.implicitConversions

import scala.annotation.tailrec

object Day20 {
  val lineRe = """p=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>,\s*v=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>,\s*a=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>""".r

  trait Number[T] extends Ordered[T] {
    def *(v: T): T
    def *(v: Int): T
    def /(v: T): T
    def /(v: Int): T
    def +(v: T): T
    def -(v: T): T
    def `unary_-`: T
    def %(v: T): T
    def >(v: Int): Boolean
    def abs: T
    def sqrt: Option[T]
  }

  case class Vector[T <% Number[T]](x: T, y: T, z: T) {
    def *(v: T) = Vector[T](x * v, y * v, z * v)
    def *(v: Int) = Vector[T](x * v, y * v, z * v)
    def +(v: Vector[T]) = copy(x + v.x, y + v.y, z + v.z)
    def /(v: T) = Vector[T](x / v, y / v, z / v)
    def /(v: Int) = Vector[T](x / v, y / v, z / v)
    def ==(v: Vector[T]) = x == v.x && y == v.y && z == v.z
  }
  object Vector {
    def abs[T <% Number[T]](v: Vector[T]): T = v.x.abs + v.y.abs + v.z.abs
  }

  import Vector._

  case class Solution[T <% Number[T]](finite: Boolean, value: Option[T])

  case class Particle[T <% Number[T]](id: Int, p: Vector[T], v: Vector[T], a: Vector[T]) {
    def step = copy(p = p + v + a, v = v + a)

    def apply(t: Int): Particle[T] = copy(p = (p + (v * 2 + a * t + a) * t / 2), v = (a * t + v))

    def collide(other: Particle[T]): Option[T] = {
      def solve(p1: T, v1: T, a1: T, p2: T, v2: T, a2: T) = {
        if (a1 == a2) {
          if (v1 == v2) {
            if (p1 == v2)
              Solution[T](false, None)
            else
              Solution[T](true, None)
          } else {
            val n = p1 - p2
            val d = v2 - v1
            if (n % d == 0)
              Solution[T](true, Some(n / d))
            else
              Solution[T](true, None)
          }
        } else {
          val det = (v2 * v2 * 4 + (- v1 * 8 + a2 * 4 - a1 * 4) + v1 * v1 * 4 + (a1 * 4 - a2 * 4) * v1 + (a2 * 8 - a1 * 8) * p2 + (a1 * 8 - a2 * 8) * p1 + a2 * a2 - a1 * a2 * 2 + a1 * a1).sqrt
          det match {
            case None => Solution[T](true, None)
            case Some(det) => {
              val ext = v2 * 2 - v1 * 2 + a2 - a1
              val d = a2 * 2 - a1 * 2

              val n1 = det - ext
              if (n1 / d > 0 && n1 % d == 0)
                Solution[T](true, Some(n1 / d))
              else {
                val n2 = det + ext
                if (n2 / d > 0 && n2 % d == 0)
                  Solution[T](true, Some(n2 / d))
                else
                  Solution[T](true, None)
              }
            }
          }
        }
      }

      solve(p.x, v.x, a.x, other.p.x, other.v.x, other.a.x) match {
        case Solution(true, None) => None
        case Solution(true, Some(t1)) =>
          solve(p.y, v.y, a.y, other.p.y, other.v.y, other.a.y) match {
            case Solution(true, None) => None
            case Solution(true, Some(t2)) if (t1 == t2) => solve(p.z, v.z, a.z, other.p.z, other.v.z, other.a.z) match {
              case Solution(true, None) => None
              case Solution(true, Some(t3)) if (t1 == t3) => Some(t1)
              case Solution(true, Some(_)) => None
              case Solution(false, None) => Some(t1)
            }
            case Solution(true, Some(_)) => None
            case Solution(false, None) => solve(p.z, v.z, a.z, other.p.z, other.v.z, other.a.z) match {
              case Solution(true, None) => None
              case Solution(true, Some(t3)) if (t1 == t3) => Some(t1)
              case Solution(true, Some(_)) => None
              case Solution(false, None) => Some(t1)
            }
            case Solution(false, Some(t2)) => ???
          }
        case Solution(false, None) => Some(p.x - p.x) // LOL
        case Solution(false, Some(t1)) => ???
      }
    }
  }

  def parse[T <% Number[T]](lines: List[String])(implicit f: (String) => T) = {
    def parse(id: Int, line: String) = line match {
      case lineRe(px, py, pz, vx, vy, vz, ax, ay, az) => Particle[T](id, Vector[T](f(px), f(py), f(pz)), Vector[T](f(vx), f(vy), f(vz)), Vector[T](f(ax), f(ay), f(az)))
    }

    lines
      .zipWithIndex
      .map(p => parse(p._2, p._1))
  }

  def solvePart1[T <% Number[T]](particles: List[Particle[T]]) = particles
    .groupBy(p => abs(p.a))
    .minBy(p => p._1)

  def solvePart2Simulation[T <% Number[T]](particles: List[Particle[T]], limit: Int = 1000) = {
    @tailrec
    def step(t: Int = 0, current: List[Particle[T]] = particles): List[Particle[T]] =
      if (current.size <= 1 || t > limit)
        current
      else
        step(
          t + 1,
          current
            .groupBy(p => p.p)
            .filter(p => p._2.size == 1)
            .map(p => p._2)
            .toList
            .flatten
            .map(p => p.step)
        )

    step().sortBy(p => p.id)
  }

  def solvePart2Mathe[T <% Number[T]](particles: List[Particle[T]]) =
    particles
      .combinations(2)
      .map(p => (p(0), p(1), p(0).collide(p(1))))
      .toList

  implicit def toLong(str: String) = str.toLong
  implicit class LongView(l: Long) extends Number[Long] {
    def *(v: Int): Long = l * v
    def /(v: Int): Long = l / v
    def +(v: Long): Long = l + v
    def abs: Long = math.abs(l)
    def compare(that: Long): Int = l.asInstanceOf[java.lang.Long].compareTo(that)

    def /(v: Long): Long = l / v
    def >(v: Int): Boolean = l > v
    def -(v: Long): Long = l - v
    def %(v: Long): Long = l % v
    def *(v: Long): Long = l * v
    def `unary_-`: Long = -l

    def sqrt: Option[Long] = {
      if (l < 0)
        None
      else {
        val r = math.sqrt(l.toDouble)
        if (r == math.round(r))
          Some(r.toLong)
        else
          None
      }
    }
  }
  
  def main(args: Array[String]) {
    val test1 = List(
      "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
      "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"
    )

    val test2 = List(
      "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
      "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
      "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
      "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
    )

    val input = Source.fromFile("data/day20/input.txt").getLines.toList
    
    val test1Particles = parse[Long](test1)

    println("part1 test=" + solvePart1(test1Particles))

    val inputParticles = parse[Long](input)

    println("part1 input=" + solvePart1(inputParticles))

    val test2Particles = parse[Long](test2)

    println("part2 test simulation=" + solvePart2Simulation(test2Particles))

    println("part2 input simulation=" + solvePart2Simulation(inputParticles).size)

    println(solvePart2Mathe(inputParticles).filter(t => t._3 match {
      case Some(_) => true
      case None => false
    }).size)
  }
}
