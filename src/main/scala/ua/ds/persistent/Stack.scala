package ua.ds.persistent

import scala.annotation.tailrec
import scala.collection.AbstractIterator

sealed trait Stack[+T] {
    self =>

    import Stack._

    def push[E >: T](element: E): Stack[E] = Frame[E](element, this)

    def pop(): (Option[T], Stack[T])

    def toIterator: Iterator[T] = new AbstractIterator[T] {
        var these = self

        override def hasNext: Boolean = these != Nil

        override def next(): T = {
            val (value, nextFrame) = these.pop()
            these = nextFrame
            value.get
        }
    }

    def foreach[A](mapperFunctor: T => A): Stack[A] = reverse.fold(Nil: Stack[A])((acc, e) => acc.push(mapperFunctor(e)))

    private def reverse: Stack[T] = fold(Nil: Stack[T])((acc, e) => Frame(e, acc))

    @tailrec
    @inline
    private def fold[A](init: A)(func: (A, T) => A): A = this match {
        case Nil => init
        case Frame(elem, tail) => tail.fold(func(init, elem))(func)
    }

    def filter(predicate: T => Boolean): Stack[T] = reverse.fold(Nil: Stack[T])((acc, e) => if (predicate(e)) acc.push(e) else acc)
}

object Stack {
    def apply[T](): Stack[T] = Nil

    case object Nil extends Stack[Nothing] {
        override def pop(): (Option[Nothing], Stack[Nothing]) = (None, Nil)
    }

    final case class Frame[T](element: T, previous: Stack[T]) extends Stack[T] {
        override def pop(): (Option[T], Stack[T]) = (Some(element), previous)
    }

}
