package aoc2017

import scala.io.Source

import scala.language.implicitConversions

import scala.annotation.tailrec

object Day20 {
  val lineRe = """p=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>,\s*v=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>,\s*a=<\s*([-0-9]+),\s*([-0-9]+),\s*([-0-9]+)>""".r

  trait Number[T] extends Ordered[T] {
    def *(v: Int): T
    def /(v: Int): T
    def +(v: T): T
    def abs: T
  }

  case class Vector[T <% Number[T]](x: T, y: T, z: T) {
    def *(v: Int) = Vector[T](x * v, y * v, z * v)
    def +(v: Vector[T]) = copy(x + v.x, y + v.y, z + v.z)
    def /(v: Int) = Vector[T](x / v, y / v, z / v)
    def ==(v: Vector[T]) = x == v.x && y == v.y && z == v.z
  }
  object Vector {
    def abs[T <% Number[T]](v: Vector[T]): T = v.x.abs + v.y.abs + v.z.abs
  }

  import Vector._

  case class Particle[T <% Number[T]](id: Int, p: Vector[T], v: Vector[T], a: Vector[T]) {
    def apply(t: Int): Particle[T] = copy(p = (p + (v * 2 + a * t + a) * t / 2), v = (a * t + v))

    def step = copy(p = p + v + a, v = v + a)
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

  def solvePart2[T <% Number[T]](particles: List[Particle[T]]) = {
    @tailrec
    def step(t: Int = 0, current: List[Particle[T]] = particles): List[Particle[T]] =
      if (current.size <= 1 || t > 100000)
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

  implicit def toLong(str: String) = str.toLong
  implicit class LongView(l: Long) extends Number[Long] {
    def *(v: Int): Long = l * v
    def /(v: Int): Long = l / v
    def +(v: Long): Long = l + v
    def abs: Long = math.abs(l)
    def compare(that: Long): Int = l.asInstanceOf[java.lang.Long].compareTo(that)
  }

  implicit def toBigInt(str: String) = BigInt(str)
  implicit class BigIntView(l: BigInt) extends Number[BigInt] {
    def *(v: Int): BigInt = l * v
    def /(v: Int): BigInt = l / v
    def +(v: BigInt): BigInt = l + v
    def abs: BigInt = l.abs
    def compare(that: BigInt) = l.compare(that)
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

    val inputParticles = parse[BigInt](input)

    println("part1 input=" + solvePart1(inputParticles))

    val test2Particles = parse[Long](test2)

    println("part2 test=" + solvePart2(test2Particles))

    println("part2 input=" + solvePart2(inputParticles).size)
  }
}
