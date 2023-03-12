import spire.syntax.cfor._

@main def hello: Unit =
  println("Hello world!")
  println(msg)
  cforRange(1 to 3) { i =>
    cforRange(1 to 3) { j =>
      println(s"i = $i, j = $j")
    }
  }
  val xs: Seq[Int] = Seq(1, 2)
  println(xs)
  val ys: Map[Int, Int] = Map()
  println(ys)

def msg = "I was compiled by Scala 3. :)"
