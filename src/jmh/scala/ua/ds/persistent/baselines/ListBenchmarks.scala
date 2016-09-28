package ua.ds.persistent.baselines

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, OutputTimeUnit, Scope, State}

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ListBenchmarks {

    val smallList: List[Int] = (0 until 100).toList
    val mediumList: List[Int] = (0 until 1000).toList
    val largeList: List[Int] = (0 until 10000).toList

    @Benchmark
    def mapSmallList(): List[String] = {
        smallList.map(e => e.toString)
    }

    @Benchmark
    def mapMediumList(): List[String] = {
        mediumList.map(e => e.toString)
    }

    @Benchmark
    def mapLargeList(): List[String] = {
        largeList.map(e => e.toString)
    }
}
