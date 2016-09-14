package ua.ds.persistent

sealed trait Stack[T] {
    def peek: Option[T]

    def size(): Int

    def push(element: T): Stack[T] = Stack.Frame[T](size() + 1, element, this)

    def pop(): Stack[T]

    def toIterator: Iterator[T] = {
        def collect(frame: Stack[T], list: List[T]): List[T] = {
            frame.peek match {
                case None => list
                case Some(value) => value :: collect(frame.pop(), list)
            }
        }

        collect(this, List()).toIterator
    }

    def foreach[A](mapperFunctor: T => A): Stack[A]

    def filter(filterFunctor: (T) => Boolean): Stack[T]
}

object Stack {
    def apply[T](): Stack[T] = Stack.Nil()

    final case class Nil[T]() extends Stack[T] {
        override def peek: Option[T] = None

        override def size(): Int = 0

        override def pop(): Stack[T] = this

        override def foreach[A](mapperFunctor: (T) => A): Stack[A] = Nil[A]()

        override def filter(filterFunctor: (T) => Boolean): Stack[T] = this
    }

    final case class Frame[T](size: Int, element: T, pop: Stack[T]) extends Stack[T] {
        override def peek: Option[T] = Some(element)

        override def foreach[A](mapperFunctor: T => A): Stack[A] = {
            Frame[A](size, mapperFunctor(element), pop.foreach(mapperFunctor))
        }

        override def filter(filterFunctor: (T) => Boolean): Stack[T] = {
            val elem = pop.filter(filterFunctor)
            peek match {
                case Some(v) if filterFunctor(v) => Frame(elem.size + 1, v, elem)
                case _ => elem
            }
        }
    }

}
