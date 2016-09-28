package ua.ds.persistent


import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.mutable.ArrayBuffer

sealed trait List[+T] {
    self =>

    import ua.ds.persistent.List._

    def zipWith[E >: T](other: List[E])(zipper: (E, E) => E): List[E] = {

//            Benchmark                               Mode  Cnt    Score    Error  Units
//            ListBenchmarks.zipLargeLists            avgt   10  175.325 ±  3.684  us/op
//            ListBenchmarks.zipMediumLists           avgt   10   17.864 ±  0.465  us/op
//            ListBenchmarks.zipSmallLists            avgt   10    1.783 ±  0.120  us/op

        val buffer: ArrayBuffer[E] = new ArrayBuffer(size)

        @tailrec
        @inline
        def loop(origin: List[E], other: List[E]): List[E] = {
            (origin, other) match {
                case (Cons(_, elem1, originTail), Cons(_, elem2, otherTail)) => buffer += zipper(elem1, elem2); loop(originTail, otherTail)
                case (_, _) => Nil
            }
        }

        loop(this, other)
        var counter = size - 1
        var result: List[E] = Nil
        while (counter > -1) {
            result = result.addToHead(buffer(counter))
            counter -= 1
        }

        result
    }

    def flatMap[A]()(map: T => List[A]): List[A] = {
        this match {
            case Nil => Nil
            case Cons(_, elem, tail) => map(elem).concatenate(tail.flatMap()(map))
        }
    }

    def filter()(predicate: T => Boolean): List[T] = {
        this match {
            case Nil => Nil
            case Cons(size, elem, tail) =>
                val last = tail.filter()(predicate)
                if (predicate(elem)) Cons(last.size + 1, elem, last)
                else last
        }
    }

    def map[A]()(func: T => A): List[A] = {
//            Benchmark                               Mode  Cnt    Score    Error  Units
//            ListBenchmarks.mapLargeList             avgt   10  466.244 ± 36.919  us/op
//            ListBenchmarks.mapMediumList            avgt   10   42.511 ±  6.463  us/op
//            ListBenchmarks.mapSmallList             avgt   10    3.389 ±  0.204  us/op
//            baselines.ListBenchmarks.mapLargeList   avgt   10  399.827 ± 92.069  us/op
//            baselines.ListBenchmarks.mapMediumList  avgt   10   34.289 ±  2.212  us/op
//            baselines.ListBenchmarks.mapSmallList   avgt   10    3.188 ±  0.205  us/op

        val buffer: ArrayBuffer[A] = new ArrayBuffer(size)

        @tailrec
        @inline
        def loop(origin: List[T]): List[A] = {
            origin match {
                case Cons(_, elem, tail) =>
                    buffer += func(elem)
                    loop(tail)
                case _ => Nil
            }
        }

        loop(this)
        var counter = size - 1
        var result: List[A] = Nil
        while (counter > -1) {
            result = result.addToHead(buffer(counter))
            counter -= 1
        }

        result
    }

    def fold[E >: T](init: E)(func: (E, E) => E): E

    def dropWhile()(predicate: (T) => Boolean): List[T]

    def drop(size: Int): List[T]

    def setHead[E >: T](element: E): List[E]

    def addToHead[E >: T](element: E): List[E]

    def addToTail[E >: T](element: E): List[E]

    def isEmpty: Boolean

    def size: Int

    def contains[E >: T](element: E): Boolean

    def head: Option[T]

    def tail: List[T]

    def toIterator: Iterator[T] = new AbstractIterator[T] {
        var these = self

        override def next(): T = these match {
            case Nil => throw new NoSuchElementException
            case Cons(_, elem, tail) => these = tail; elem
        }

        override def hasNext: Boolean = these.size != 0
    }

    def concatenate[E >: T](other: List[E]): List[E] = {
        this match {
            case Nil => other
            case Cons(size, elem, tail) => Cons(size + other.size, elem, tail.concatenate(other))
        }
    }
}

object List {
    def apply[T](): List[T] = Nil

    def apply[T](seq: T*): List[T] = {
        iteration[T](seq.reverseIterator, Nil)
    }

    def apply(range: Range): List[Int] = {
        iteration[Int](range.reverseIterator, Nil)
    }

    @tailrec
    private def iteration[T](iterator: Iterator[T], list: List[T]): List[T] = {
        if (!iterator.hasNext) list
        else iteration(iterator, list.addToHead(iterator.next()))
    }

    case object Nil extends List[Nothing] {
        override def fold[E >: Nothing](init: E)(func: (E, E) => E): E = init

        override def dropWhile()(predicate: (Nothing) => Boolean): List[Nothing] = this

        override def drop(size: Int): List[Nothing] = this

        override def setHead[E >: Nothing](element: E): List[E] = addToHead(element)

        override def addToHead[E](element: E): List[E] = Cons(1, element, Nil)

        override def addToTail[E >: Nothing](element: E): List[E] = List.Cons(1, element, List.Nil)

        override def isEmpty: Boolean = true

        override def size: Int = 0

        override def contains[E](element: E): Boolean = false

        override def head: Option[Nothing] = None

        def tail: List[Nothing] = List.Nil
    }

    final case class Cons[+T](size: Int, elem: T, tail: List[T]) extends List[T] {
        override def fold[E >: T](init: E)(func: (E, E) => E): E = {
            tail.fold(func(init, elem))(func)
        }

        override def dropWhile()(predicate: (T) => Boolean): List[T] = {
            if (predicate(elem)) tail.dropWhile()(predicate)
            else this
        }

        override def drop(size: Int): List[T] = {
            if (size == 0) this
            else tail.drop(size - 1)
        }

        override def setHead[E >: T](element: E): List[E] = Cons(size, element, tail)

        override def addToHead[E >: T](element: E): List[E] = Cons(size + 1, element, this)

        override def addToTail[E >: T](element: E): List[E] = Cons(size + 1, this.elem, tail.addToTail(element))

        override def isEmpty: Boolean = false

        override def contains[E >: T](element: E): Boolean = {
            if (element == elem) true
            else tail.contains(element)
        }

        override def head: Option[T] = Some(elem)
    }

}
