package ua.ds.persistent

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.mutable.ArrayBuffer

sealed trait List[+T] {
    self =>

    import List._

    def scan[A](init: A)(operation: (A, T) => A): List[A] = this match {
        case Nil => Nil
        case Cons(elem, tail) => val v = operation(init, elem); tail.scan(v)(operation).addToHead(v)
    }

    @tailrec
    @inline
    final def exists(predicate: T => Boolean): Boolean = this match {
        case Nil => false
        case Cons(elem, tail) =>
            if (predicate(elem)) true
            else tail.exists(predicate)
    }

    @tailrec
    @inline
    final def forall(predicate: T => Boolean): Boolean = this match {
        case Nil => true
        case Cons(elem, tail) =>
            if (!predicate(elem)) false
            else tail.forall(predicate)
    }

    def takeWhile()(predicate: T => Boolean): List[T] = this match {
        case Cons(elem, tail) if predicate(elem) => Cons(elem, tail.takeWhile()(predicate))
        case _ => Nil
    }

    def take(size: Int): List[T] = this match {
        case Cons(elem, tail) if size > 0 => Cons(elem, tail.take(size - 1))
        case _ => Nil
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

    @tailrec
    @inline
    final def dropWhile()(predicate: (T) => Boolean): List[T] = this match {
        case Nil => Nil
        case Cons(elem, tail) =>
            if (!predicate(elem)) this
            else tail.dropWhile()(predicate)
    }

    @tailrec
    @inline
    final def drop(size: Int): List[T] = this match {
        case Nil => Nil
        case Cons(elem, tail) =>
            if (size == 0) this
            else tail.drop(size - 1)
    }

    @inline
    def setHead[E >: T](element: E): List[E] = this match {
        case Nil => addToHead(element)
        case Cons(_, tail) => Cons(element, tail)
    }

    @inline
    def addToHead[E >: T](element: E): List[E] = Cons(element, this)

    def addToTail[E >: T](element: E): List[E] = {
        val buffer = new ArrayBuffer[E]()

        val these = toIterator
        while (these.hasNext) {
            buffer += these.next
        }
        buffer += element

        buffer.foldRight(Nil: List[E])((e, acc) => acc.addToHead(e))
    }

    @tailrec
    @inline
    final def contains[E >: T](element: E): Boolean = this match {
        case Nil => false
        case Cons(elem, tail) =>
            if (element == elem) true
            else tail.contains(element)
    }

    def concatenate[E >: T](other: List[E]): List[E] = reverse.fold(other)((acc, e) => acc.addToHead(e))

    def toIterator: Iterator[T] = new AbstractIterator[T] {
        var these = self

        override def next(): T = these match {
            case Nil => throw new NoSuchElementException
            case Cons(elem, tail) => these = tail; elem
        }

        override def hasNext: Boolean = !these.isEmpty
    }

    def isEmpty: Boolean = this match {
        case Nil => true
        case Cons(_, _) => false
    }

    def head: Option[T]

    def tail: List[T]
}

object List {
    def apply[T](): List[T] = Nil

    def apply[T](seq: T*): List[T] = seq.foldRight(Nil: List[T])((e, acc) => acc.addToHead(e))

    def apply(range: Range): List[Int] = range.foldRight(Nil: List[Int])((e, acc) => acc.addToHead(e))

    case object Nil extends List[Nothing] {
        override def head: Option[Nothing] = None

        override def tail: List[Nothing] = Nil
    }

    final case class Cons[+T](elem: T, tail: List[T]) extends List[T] {
        override def head: Option[T] = Some(elem)
    }

}
