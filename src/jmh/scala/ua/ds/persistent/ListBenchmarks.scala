package ua.ds.persistent

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, OutputTimeUnit, Scope, State}

@State(Scope.Benchmark)
class ListBenchmarks {

    val smallList: List[Int] = List(0 until 100)
    val mediumList: List[Int] = List(0 until 1000)
    val largeList: List[Int] = List(0 until 10000)

    @Benchmark
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def mapSmallList(): List[String] = {
        smallList.map()(e => e.toString)
    }

    @Benchmark
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def mapMediumList(): List[String] = {
        mediumList.map()(e => e.toString)
    }

    @Benchmark
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def mapLargeList(): List[String] = {
        largeList.map()(e => e.toString)
    }
}
