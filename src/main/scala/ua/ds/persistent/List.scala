package ua.ds.persistent

sealed abstract class List {
    def add(element: Int): List

    def isEmpty: Boolean

    def size(): Int

    def contains(element: Int): Boolean

}

object List {
    def apply() = Nil

    case object Nil extends List {
        override def add(element: Int): List = Cons(1, element, Nil)

        override def isEmpty: Boolean = true

        override def size(): Int = 0

        override def contains(element: Int): Boolean = false
    }

    final case class Cons(size: Int, element: Int, tail: List) extends List {
        override def add(element: Int): List = Cons(size + 1, element, this)

        override def isEmpty: Boolean = false

        override def contains(element: Int): Boolean = {
            if (element == this.element) true
            else tail.contains(element)
        }
    }
}
