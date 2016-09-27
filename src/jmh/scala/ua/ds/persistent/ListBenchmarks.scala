package ua.ds.persistent

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class ListBenchmarks {

    val list: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

    @Benchmark
    def mapList(): List[String] = {
        list.map[String]()(e => e.toString)
    }
}
