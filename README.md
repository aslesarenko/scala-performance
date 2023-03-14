# Scala-performance guidance 

This repository contains a supplemental code for my article [Scala Code Performance For No Efforts]().

### Examples of optimization

The examples of optimization are located in [src/main/scala/examples](src/main/scala/examples).

### Benchmarks

The benchmarks are located in [BasicBenchmarks.scala](src/test/scala/benchmarks/BasicBenchmarks.scala). 
They are written using the [Scalameter](https://github.com/scalameter/scalameter) framework.
Run the benchmarks with the following command:

```bash
sbt "Test/runMain benchmarks.Benchmark"
```

Follow the instructions in the ScalaDoc of `Benchmark` class to add your own benchmarks.

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt
run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
