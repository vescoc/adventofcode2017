package aoc2017

object Benchmark {
  def bench[T](f: => T): T = b((delta: Long) => s"time=${delta}ms/${delta / 1000}s", f)

  def bench[T](msg: String)(f: => T): T = b((delta: Long) => s"$msg time=${delta}ms/${delta / 1000}s", f)

  private def b[T](p: (Long) => String, f: => T): T = {
    val startTime = System.currentTimeMillis

    val r = f

    val endTime = System.currentTimeMillis

    println(p(endTime - startTime))

    r
  }
}
