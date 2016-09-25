package ua.ds.persistent

sealed abstract class List[+T] {
    def add[E >: T](element: E): List[E]

    def isEmpty: Boolean

    def size(): Int

    def contains[E >: T](element: E): Boolean

    def toIterator: Iterator[T] = ???

    def concatenate[E >: T](other: List[E]): List[E] = ???

    def addToTail[E >: T](element: E): List[E] = ???

}

object List {
    def apply[T](): List[T] = Nil

    case object Nil extends List[Nothing] {
        override def add[E](element: E): List[E] = Cons(1, element, Nil)

        override def isEmpty: Boolean = true

        override def size(): Int = 0

        override def contains[E](element: E): Boolean = false
    }

    final case class Cons[+T](size: Int, element: T, tail: List[T]) extends List[T] {
        override def add[E >: T](element: E): List[E] = Cons(size + 1, element, this)

        override def isEmpty: Boolean = false

        override def contains[E >: T](element: E): Boolean = {
            if (element == this.element) true
            else tail.contains(element)
        }
    }
}
