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

  class Mon(id: String) {
    private var _min = 0L
    private var _max = 0L
    private var _count = 0
    private var _total = 0L

    def apply[T](f: => T) = {
      val startTime = System.currentTimeMillis

      val r = f

      val endTime = System.currentTimeMillis

      val delta = endTime - startTime

      _min = math.min(_min, delta)
      _max = math.max(_max, delta)
      _count = count + 1
      _total = _total + delta

      r
    }

    def min = _min
    def max = _max
    def count = _count
    def mean = (
      if (count == 0)
        0.0
      else
        total.toDouble / count
    )
    def total = _total

    def info = f"$id min=${min}ms max=${max}ms count=$count total=${total}ms mean=${mean}%.5gms"
  }
  object Mon {
    def apply(id: String) = new Mon(id)
  }
}
