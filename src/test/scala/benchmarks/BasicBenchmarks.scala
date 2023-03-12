package benchmarks

import examples.EditDistance
import org.scalameter.{KeyValue, log, CurveData}
import org.scalameter.api.*
import org.scalameter.utils.Tree
import spire.syntax.cfor.*

import scala.annotation.tailrec
import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.Seq
import scala.util.Random

trait BenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>
  private val configForSeqVsArray = Seq[KeyValue](
    KeyValue(exec.benchRuns -> 50),
    KeyValue(exec.minWarmupRuns -> 30),
    KeyValue(exec.maxWarmupRuns -> 50),
    KeyValue(exec.requireGC -> true)
  )

  performance.of("Boxed vs Unboxed Array[Int]").config(configForSeqVsArray:_*) in {
    var s = 0
    measure method "boxed" in {
      using(sizes) in { case n =>
        val arr = new Array[Integer](n)
        cfor(0)(_ < n, _ + 1) { i =>
          arr(i) = Integer.valueOf(i)
        }
        s = 0
        cfor(0)(_ < n, _ + 1) { i =>
          s += arr(i).intValue()
        }
      }
    }
    measure method "unboxed" in {
      using(sizes) in { case n =>
        val arr = new Array[Int](n)
        cfor(0)(_ < n, _ + 1) { i =>
          arr(i) = i
        }
        s = 0
        cfor(0)(_ < n, _ + 1) { i =>
          s += arr(i)
        }
      }
    }
  }

  performance.of("Seq").config(configForSeqVsArray:_*) in {
    var res: Seq[Int] = null
    measure method "Seq()" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Seq() }
      }
    }
    measure method "Nil" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Nil }
      }
    }
  }

  performance.of("Map").config(configForSeqVsArray:_*) in {
    var res: Map[Int,Int] = null
    measure method "Map()" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ =>
          res = Map[Int,Int]()
        }
      }
    }
    measure method "Map.empty" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Map.empty[Int, Int] }
      }
    }
  }

  performance.of("for comprehension").config(configForSeqVsArray:_*) in {
    var cell: Int = 0
    measure method "foreach" in {
      using(arrays) in { case (xs, _) =>
        xs.foreach { x => cell = x }
      }
    }
    measure method "cfor" in {
      using(arrays) in { case (xs, _) =>
        cfor(0)(_ < xs.length, _ + 1) { i => cell = xs(i) }
      }
    }
  }

  performance.of("Seq vs Array (from elems)").config(configForSeqVsArray: _*) in {
    var res: Seq[Int] = null
    measure method "Seq" in {
      using(sizes) in { n =>
        cfor(0)(_ < n, _ + 1) { i =>
          res = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, i)
        }
      }
    }
    measure method "Array" in {
      using(sizes) in { n =>
        cfor(0)(_ < n, _ + 1) { i =>
          res = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, i)
        }
      }
    }
  }

  performance.of("Seq vs Array (from Range)").config(configForSeqVsArray:_*) in {
    var res: Seq[Int] = null
    val nIters = 100
    measure method "Seq" in {
      using(sizes) in { n =>
        var range = (0 until n)
        cfor(0)(_ < nIters, _ + 1) { _ =>
          res = Seq(range: _*)
        }
      }
    }
    measure method "Array" in {
      using(sizes) in { n =>
        var range = (0 until n)
        cfor(0)(_ < nIters, _ + 1) { _ =>
          res = Array(range:_*)
        }
      }
    }
  }

  performance.of("Seq vs Array (list ops)").config(configForSeqVsArray:_*) in {
    var res: Int = 0
    val nIters = 2
    @tailrec def sum(xs: Seq[Int], accum: Int = 0): Int = {
      if (xs.isEmpty) {
        accum
      } else {
        sum(xs.tail, accum + xs.head)
      }
    }

    measure method "Seq" in {
      using(lowSizes) in { n =>
        var range = Seq((0 until n): _*)
        cfor(0)(_ < nIters, _ + 1) { _ =>
          res = sum(range)
        }
      }
    }
    measure method "Array" in {
      using(lowSizes) in { n =>
        var range: Seq[Int] = Array(0 until n :_*)
        cfor(0)(_ < nIters, _ + 1) { _ =>
          res = sum(range)
        }
      }
    }
  }

//  performance.of("edit distance").config(configForSeqVsArray: _*) in {
//    measure method "distance" in {
//      var res: Int = 0
//      using(lowSizes) in { n =>
//        val s = randomString(n)
//        val t = randomString(n)
//        res = EditDistance.distance(s, t)
//      }
//    }
//    measure method "distance_opt" in {
//      var res: Int = 0
//      using(lowSizes) in { n =>
//        val s = randomStringOpt(n)
//        val t = randomStringOpt(n)
//        res = EditDistance.distanceOpt(s, t)
//      }
//    }
//  }

  def randomString(length: Int): String = {
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') // Define valid characters
    val random = new Random()
    (1 to length).map(_ => chars(random.nextInt(chars.length))).mkString
  }

  def randomStringOpt(length: Int): String = {
    val chars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toArray // Define valid characters
    val random = new Random()
    val buf = new Array[Char](length)
    cforRange(0 until length) { i =>
      buf(i) = chars(random.nextInt(chars.length))
    }
    buf.mkString
  }

}

object FastBenchmark extends Bench.LocalTime with BenchmarkCases {
  val r = new LoggingReporter[Double]
  override def reporter: Reporter[Double] = new Reporter[Double] {
    override def report(result: CurveData[Double], persistor: Persistor): Unit =
      r.report(result, persistor)

    override def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = {
      r.report(results, persistor)
      def headerLine(slowTitle: String, fastTitle: String) =
        log.report(s"| Size       | ${(slowTitle + ", ms").padTo(13, ' ')}| ${(fastTitle + ", ms").padTo(13, ' ')}| Speedup    |")
      def separatorLine() =
        log.report(s"|------------|--------------|--------------|------------|")

      for (benchmark <- results.children) {
        assert(benchmark.children.size == 2, s"There should be two methods in ${benchmark.context.scope}")
        val benchmarkName = benchmark.context.scope
        val slow = benchmark.children(0)
        val fast = benchmark.children(1)

        log.report(s"## Benchmark: $benchmarkName \n")

        headerLine(slow.context.scopeList.last, fast.context.scopeList.last)
        separatorLine()
        slow.items(0).measurements.zip(fast.items(0).measurements).foreach { case (slow, fast) =>
          val slowTime = slow.value
          val fastTime = fast.value
          val size = slow.params.axisData.find(_._1.fullName == "size").get._2.asInstanceOf[Int]
          val ratio = slowTime / fastTime
          val ratioStr = f"$ratio%10.2f"
          log.report(f"| ${size.toString.padTo(11, ' ')}| $slowTime%12.7f | $fastTime%12.7f | ${ratioStr.padTo(10, ' ')} |")
        }
        separatorLine()
        log.report("")
      }
      true
    }
  }
}

