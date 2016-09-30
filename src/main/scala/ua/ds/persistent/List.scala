package ua.ds.persistent

import scala.annotation.tailrec
import scala.collection.AbstractIterator

sealed trait List[+T] {
    self =>

    import ua.ds.persistent.List._

    @tailrec
    @inline
    final def forall(predicate: T => Boolean): Boolean = {
        this match {
            case Cons(elem, tail) => if (predicate(elem)) tail.forall(predicate) else false
            case Nil => true
        }
    }

    def takeWhile()(predicate: T => Boolean): List[T] = {
        this match {
            case Cons(elem, tail) if predicate(elem) => Cons(elem, tail.takeWhile()(predicate))
            case _ => Nil
        }
    }

    def take(size: Int): List[T] = {
        this match {
            case Cons(elem, tail) if size > 0 => Cons(elem, tail.take(size - 1))
            case _ => Nil
        }
    }

    def reverse: List[T] = fold(Nil: List[T])((acc, e) => Cons(e, acc))

    def map[A]()(func: T => A): List[A] = reverse.fold(Nil: List[A])((acc, e) => acc.addToHead(func(e)))

    def filter()(predicate: T => Boolean): List[T] = reverse.fold(Nil: List[T])((acc, e) => if (predicate(e)) acc.addToHead(e) else acc)

    def flatMap[A]()(map: T => List[A]): List[A] = reverse.fold(Nil: List[A])((acc, e) => map(e).concatenate(acc))

    def zipWith[E >: T, A, B](other: List[A])(zipper: (E, A) => B): List[B] = {
        var result: List[B] = Nil
        val these = this.toIterator
        val those = other.toIterator
        while (these.hasNext && those.hasNext) {
            result = result.addToHead(zipper(these.next(), those.next()))
        }
        result.reverse
    }

    @tailrec
    @inline
    final def fold[A](init: A)(func: (A, T) => A): A = this match {
        case Nil => init
        case Cons(elem, tail) => tail.fold(func(init, elem))(func)
    }

    def dropWhile()(predicate: (T) => Boolean): List[T]

    def drop(size: Int): List[T]

    def setHead[E >: T](element: E): List[E]

    def addToHead[E >: T](element: E): List[E]

    def addToTail[E >: T](element: E): List[E]

    def isEmpty: Boolean

    def contains[E >: T](element: E): Boolean

    def head: Option[T]

    def tail: List[T]

    def toIterator: Iterator[T] = new AbstractIterator[T] {
        var these = self

        override def next(): T = these match {
            case Nil => throw new NoSuchElementException
            case Cons(elem, tail) => these = tail; elem
        }

        override def hasNext: Boolean = !these.isEmpty
    }

    def concatenate[E >: T](other: List[E]): List[E] = reverse.fold(other)((acc, e) => acc.addToHead(e))
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
    @inline
    private def iteration[T](iterator: Iterator[T], list: List[T]): List[T] = {
        if (!iterator.hasNext) list
        else iteration(iterator, list.addToHead(iterator.next()))
    }

    case object Nil extends List[Nothing] {
        override def dropWhile()(predicate: (Nothing) => Boolean): List[Nothing] = Nil

        override def drop(size: Int): List[Nothing] = Nil

        override def setHead[E >: Nothing](element: E): List[E] = addToHead(element)

        override def addToHead[E](element: E): List[E] = Cons(element, Nil)

        override def addToTail[E >: Nothing](element: E): List[E] = Cons(element, Nil)

        override def isEmpty: Boolean = true

        override def contains[E](element: E): Boolean = false

        override def head: Option[Nothing] = None

        override def tail: List[Nothing] = Nil
    }

    final case class Cons[+T](elem: T, tail: List[T]) extends List[T] {
        override def dropWhile()(predicate: (T) => Boolean): List[T] = {
            if (predicate(elem)) tail.dropWhile()(predicate)
            else this
        }

        override def drop(size: Int): List[T] = {
            if (size == 0) this
            else tail.drop(size - 1)
        }

        override def setHead[E >: T](element: E): List[E] = Cons(element, tail)

        override def addToHead[E >: T](element: E): List[E] = Cons(element, this)

        override def addToTail[E >: T](element: E): List[E] = Cons(this.elem, tail.addToTail(element))

        override def isEmpty: Boolean = false

        override def contains[E >: T](element: E): Boolean = {
            if (element == elem) true
            else tail.contains(element)
        }

        override def head: Option[T] = Some(elem)
    }

}
