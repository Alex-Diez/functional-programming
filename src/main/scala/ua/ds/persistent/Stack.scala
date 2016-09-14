package ua.ds.persistent

sealed trait Stack[T] {
    def peek: Option[T]

    def size(): Int

    def push(element: T): Stack[T] = Stack.NoneEmptyStack[T](size() + 1, Some(element), this)

    def pop(): Stack[T]

    def toIterator: Iterator[Option[T]] = StackIterator[T](this)

    def foreach[A](mapperFunctor: T => A): Stack[A]

    def filter(filterFunctor: (T) => Boolean): Stack[T]
}

object Stack {
    def apply[T](): Stack[T] = Stack.Nil()

    final case class Nil[T]() extends Stack[T] {
        override def peek: Option[T] = None

        override def size(): Int = 0

        override def pop(): Stack[T] = Nil[T]()

        override def foreach[A](mapperFunctor: (T) => A): Stack[A] = Nil[A]()

        override def filter(filterFunctor: (T) => Boolean): Stack[T] = Nil[T]()
    }

    final case class NoneEmptyStack[T](size: Int, peek: Option[T], pop: Stack[T]) extends Stack[T] {
        override def foreach[A](mapperFunctor: T => A): Stack[A] = {
            NoneEmptyStack[A](size, peek.map(mapperFunctor), pop.foreach(mapperFunctor))
        }

        override def filter(filterFunctor: (T) => Boolean): Stack[T] = {
            val elem = pop.filter(filterFunctor)
            peek match {
                case Some(v) if filterFunctor(peek.get) => NoneEmptyStack(elem.size + 1, peek, elem)
                case _ => elem
            }
        }
    }
}

private case class StackIterator[T](var elem: Stack[T]) extends Iterator[Option[T]] {
    override def hasNext: Boolean = elem.size() != 0

    override def next(): Option[T] = {
        val r = elem.peek
        elem = elem.pop()
        r
    }
}
