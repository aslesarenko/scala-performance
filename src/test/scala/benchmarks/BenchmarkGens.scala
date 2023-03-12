package benchmarks

import org.scalameter.KeyValue
import org.scalameter.api.{Gen, Bench, *}

trait BenchmarkGens { suite: Bench[Double] =>
  def maxSize = 100000

  val highSizes = Gen.exponential("size")(100, maxSize * 10, 10)
  val sizes = Gen.exponential("size")(10, maxSize, 10)
  val lowSizes = Gen.exponential("size")(1, maxSize / 10, 10)

  val ranges = for { size <- sizes } yield (0 until size, maxSize / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }
}
