package ua.ds.persistent

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, OutputTimeUnit, Scope, State}

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ListBenchmarks {

    val firstSmallList = List(0 until 100)
    val secondSmallList = List(300 until 400)
    val firstMediumList = List(0 until 1000)
    val secondMediumList = List(3000 until 4000)
    val firstLargeList = List(0 until 10000)
    val secondLargeList = List(30000 until 40000)

    @Benchmark
    def mapSmallList(): List[String] = {
        firstSmallList.map()(e => e.toString)
    }

    @Benchmark
    def mapMediumList(): List[String] = {
        firstMediumList.map()(e => e.toString)
    }

    @Benchmark
    def mapLargeList(): List[String] = {
        firstLargeList.map()(e => e.toString)
    }

    @Benchmark
    def zipSmallLists(): List[Int] = {
        firstSmallList.zipWith(secondSmallList)((e1, e2) => e1 + e2)
    }

    @Benchmark
    def zipMediumLists(): List[Int] = {
        firstMediumList.zipWith(secondMediumList)((e1, e2) => e1 + e2)
    }

    @Benchmark
    def zipLargeLists(): List[Int] = {
        firstLargeList.zipWith(secondLargeList)((e1, e2) => e1 + e2)
    }

    @Benchmark
    def filterSmallList(): List[Int] = {
        firstSmallList.filter()(e => e % 10 == 0)
    }

    @Benchmark
    def filterMediumList(): List[Int] = {
        firstMediumList.filter()(e => e % 10 == 0)
    }

    @Benchmark
    def filterLargeList(): List[Int] = {
        firstLargeList.filter()(e => e % 10 == 0)
    }

    @Benchmark
    def flatMapSmallList(): List[Int] = {
        firstSmallList.flatMap()(e => List(e, e ,e))
    }

    @Benchmark
    def flatMapMediumList(): List[Int] = {
        firstMediumList.flatMap()(e => List(e, e ,e))
    }

    @Benchmark
    def flatMapLargeList(): List[Int] = {
        firstLargeList.flatMap()(e => List(e, e ,e))
    }
}
